module CPS.Inferer where

import Control.Monad.State
    ( MonadState(get), modify, evalStateT, StateT )
import Control.Monad.Except
    ( MonadError(throwError), runExcept, Except )
import qualified Data.Map as Map
import Data.List (nub)
import Text.Read (readMaybe)
import CPS.Typing
    ( Command(..), Context, MonoType(..), PolyType(..), Substitution )
import Lambda.Typing (Id)

-- | Type inference errors
data TypeError
  = OccursCheck String MonoType
  | Mismatch MonoType MonoType
  | ArityMismatch [MonoType] [MonoType]
  | UnboundVariable String
  deriving (Show)

-- | Type inference monad: State for fresh variables + Except for errors
type TI a = StateT Int (Except TypeError) a

greekLetters :: [String]
greekLetters = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
                "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

-- | Generate fresh type variable names using Greek letters
greekVar :: Int -> String
greekVar n =
  let (q, r) = n `divMod` length greekLetters
      suffix = if q == 0 then "" else show (q - 1)
  in greekLetters !! r ++ suffix

runTI :: TI a -> Either TypeError a
runTI m = runExcept (evalStateT m 0)

freshTVar :: TI MonoType
freshTVar = do
  n <- get
  modify (+1)
  return (TVar (greekVar n))

-- | Compute free type variables (FTV) of a monotype.
--   1. Collect variables in `TVar`
--   2. Ignore `TInt`
--   3. Recurse into `TNeg` components
ftvMono :: MonoType -> [String]
ftvMono (TVar v)     = [v]
ftvMono TInt         = []
ftvMono (TNeg ts)    = nub $ concatMap ftvMono ts

-- | Compute FTV of a polytype `∀α.τ`.
--   1. Compute FTV of `τ`
--   2. Subtract bound variables `α`
ftvPoly :: PolyType -> [String]
ftvPoly (Forall vars t) = filter (`notElem` vars) (ftvMono t)

-- | Compute FTV of a context.
--   1. Collect FTV of all polytypes in the context
ftvContext :: Context -> [String]
ftvContext ctx = nub $ concatMap ftvPoly (Map.elems ctx)

-- | Apply substitution to a monotype recursively.
--   1. Replace variables using substitution `s`
--   2. Recurse into `TNeg` components
applySubst :: Substitution -> MonoType -> MonoType
applySubst s (TVar v)    = case Map.lookup v s of
                             Just t  -> t
                             Nothing -> TVar v
applySubst _ TInt        = TInt
applySubst s (TNeg ts)   = TNeg (map (applySubst s) ts)

-- | Apply substitution to a polytype
applySubstToPoly :: Substitution -> PolyType -> PolyType
applySubstToPoly s (Forall vars t) = Forall vars (applySubst s t)

-- | Apply substitution to a context by updating all polytypes.
--   1. For each polytype in the context:
--      a. Apply substitution to its monotype
applySubstToContext :: Substitution -> Context -> Context
applySubstToContext s = Map.map (applySubstToPoly s)

-- | Compose two substitutions `s1` and `s2`.
--   1. Apply `s1` to all types in `s2`
--   2. Merge results, starting from `s1`
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

-- | Check if a variable occurs in a type (for unification)
occursCheck :: String -> MonoType -> Bool
occursCheck u (TVar v)    = u == v
occursCheck _ TInt        = False
occursCheck u (TNeg ts)   = any (occursCheck u) ts

-- | Bind a type variable to a monotype (with occurs-check).
--   1. If `u ≡ t`, return empty substitution
--   2. Check if `u` occurs in `t` (prevents cyclic types)
--   3. Return substitution `[u ↦ t]` if safe
varBind :: String -> MonoType -> TI Substitution
varBind u t
  | t == TVar u       = return Map.empty
  | occursCheck u t   = throwError $ OccursCheck u t
  | otherwise         = return (Map.singleton u t)

-- | Unify two monotypes to find the most general unifier (substitution).
--   1. If either type is a variable, bind it to the other type (with occurs-check)
--   2. Unify `TInt` with `TInt` (no substitution)
--   3. For negations `TNeg`, check arity matches, then unify component-wise
--   4. Throw `Mismatch` for all other cases
mgu :: MonoType -> MonoType -> TI Substitution
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt  = return Map.empty
mgu (TNeg ts1) (TNeg ts2)
  | length ts1 == length ts2 = mguLists ts1 ts2
  | otherwise = throwError $ ArityMismatch ts1 ts2
mgu t1 t2 = throwError $ Mismatch t1 t2

-- | Unify lists of types component-wise
mguLists :: [MonoType] -> [MonoType] -> TI Substitution
mguLists [] [] = return Map.empty
mguLists (t:ts) (s:ss) = do
  subst1 <- mgu t s
  subst2 <- mguLists (map (applySubst subst1) ts) (map (applySubst subst1) ss)
  return (composeSubst subst2 subst1)
mguLists _ _ = throwError $ ArityMismatch [] []

-- | Instantiate a polytype by replacing bound variables with fresh ones.
--   1. For each quantified variable in `∀α.τ`, generate a fresh type variable
--   2. Substitute `α` with fresh variables in `τ`
--   3. Return the resulting monotype
instantiate :: PolyType -> TI MonoType
instantiate (Forall vars t) = do
  freshVars <- mapM (const freshTVar) vars
  let s = Map.fromList (zip vars freshVars)
  return (applySubst s t)

-- | Generalize a monotype to a polytype by universally quantifying free variables.
--   1. Compute free type variables (FTV) of the monotype `t`
--   2. Subtract FTV of the current context `ctx` (to avoid capturing bound vars)
--   3. Quantify the remaining FTVs as `∀α.τ`
generalize :: Context -> MonoType -> PolyType
generalize ctx t = Forall vars t
  where
    ctxFtvs = ftvContext ctx
    vars = nub $ filter (`notElem` ctxFtvs) (ftvMono t)

-- | Infer type for an atomic term (variable/number).
--   1. If `x` is a number, return `TInt`
--   2. If `x` is a variable:
--      a. Look up its polytype in the context
--      b. Instantiate it to a fresh monotype
--   3. Return substitution and inferred monotype
inferAtom :: Context -> String -> TI (Substitution, MonoType)
inferAtom ctx x =
  case readMaybe x :: Maybe Integer of
    Just _  -> return (Map.empty, TInt)
    Nothing -> case Map.lookup x ctx of
                 Just poly -> do
                   t <- instantiate poly
                   return (Map.empty, t)
                 Nothing -> throwError $ UnboundVariable x

-- | Extend context with parameters and their types
extendContextWithParams :: Context -> [Id] -> [MonoType] -> Context
extendContextWithParams ctx ys paramTypes =
  foldl (\acc (p, τ) -> Map.insert p (Forall [] τ) acc) ctx (zip ys paramTypes)

-- | Infer types for a CPS command (Jump: `k(a₁,...,aₙ`).
--   1. Infer type `tK` for continuation `k` (substitution `s1`)
--   2. Apply `s1` to the context, then infer types for args `a₁...aₙ`
--   3. Compose substitutions from args into `sArgs`
--   4. Unify `tK` with `¬[τ₁,...,τₙ]` (where `τᵢ` are arg types)
--   5. Return total substitution `s2 ∘ s1`
inferCommand :: Context -> Command -> TI Substitution
inferCommand ctx (Jump k xs) = do
  (s1, tK) <- inferAtom ctx k
  atomResults <- mapM (inferAtom (applySubstToContext s1 ctx)) xs
  let sArgs    = foldl composeSubst Map.empty (map fst atomResults)
      argsTypes = map snd atomResults
  s2 <- mgu (applySubst sArgs tK) (TNeg argsTypes)
  return (composeSubst s2 s1)

-- | Infer types for a CPS command (Bind: `let k(x₁...xₙ) = c in b`).
--   1. Generate fresh type variables `τ₁...τₙ` for parameters `x₁...xₙ`
--   2. Extend context with `xᵢ : τᵢ`
--   3. Infer type for `c` under extended context (substitution `s1`)
--   4. Generalize `¬[τ₁...τₙ]` to polytype `σ` after applying `s1`
--   5. Extend context with `k : σ`, infer `b` (substitution `s2`)
--   6. Return composed substitution `s2 ∘ s1`
inferCommand ctx (Bind b y ys c) = do
  paramTypes <- mapM (const freshTVar) ys
  let ctxParams = extendContextWithParams ctx ys paramTypes
  s1 <- inferCommand ctxParams c
  let contType = applySubst s1 (TNeg paramTypes)
  let ctxSubst = applySubstToContext s1 ctx
  let sigma = generalize ctxSubst contType
  let ctx' = Map.insert y sigma ctxSubst
  s2 <- inferCommand ctx' b
  return (composeSubst s2 s1)

-- | Top-level type inference for a CPS command.
--   1. Initialize context with `k : α` (fresh variable)
--   2. Infer substitution `subst` for the command
--   3. Apply `subst` to the initial context
--   4. Return final substitution and context
inferWithCtx :: Command -> TI (Substitution, Context)
inferWithCtx cmd = do
  ctx <- Map.singleton "k" . Forall [] <$> freshTVar
  subst <- inferCommand ctx cmd
  return (subst, applySubstToContext subst ctx)
