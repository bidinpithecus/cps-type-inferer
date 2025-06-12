module CPS.Inferer where

import Control.Monad.State
    ( MonadState(get), modify, evalStateT, StateT )
import Control.Monad.Except
    ( MonadError(throwError), runExcept, Except )
import qualified Data.Map as Map
import Data.List (nub, intercalate)
import Text.Read (readMaybe)
import CPS.Typing
    ( Command(..), Context, CPSMonoType(..), CPSPolyType(..), Substitution, initialCont )
import Utils.Typing (greekVar, Id, greekLetters)
import qualified Data.Maybe
import Control.Monad (foldM, replicateM)

-- | Type inference errors
data TypeError
  = OccursCheck String CPSMonoType
  | Mismatch CPSMonoType CPSMonoType
  | ArityMismatch [CPSMonoType] [CPSMonoType]
  | UnboundVariable String Context

instance Show TypeError where
  show (OccursCheck v t) = "OccursCheck: " ++ v ++ " in " ++ show t
  show (Mismatch t1 t2) = "Mismatch: " ++ show t1 ++ " and " ++ show t2
  show (ArityMismatch ts1 ts2) = "ArityMismatch: expected " ++ show (length ts1) 
      ++ " args (" ++ intercalate ", " (map show ts1) 
      ++ "), got " ++ show (length ts2) ++ " (" ++ intercalate ", " (map show ts2) ++ ")"
  show (UnboundVariable v _) = "UnboundVariable: " ++ v

-- | Type inference monad: State for fresh variables + Except for errors
type TI a = StateT Int (Except TypeError) a

runTI :: TI a -> Either TypeError a
runTI m = runExcept (evalStateT m 0)

freshTVar :: TI CPSMonoType
freshTVar = do
  n <- get
  modify (+1)
  return (TVar (greekVar n))

-- | Compute free type variables (FTV) of a monotype.
--   1. Collect variables in `TVar`
--   2. Ignore `TInt`
--   3. Recurse into `TNeg` components
ftvMono :: CPSMonoType -> [String]
ftvMono (TVar v)     = [v]
ftvMono TInt         = []
ftvMono (TNeg ts)    = nub $ concatMap ftvMono ts

-- | Compute FTV of a polytype `∀α.τ`.
--   1. Compute FTV of `τ`
--   2. Subtract bound variables `α`
ftvPoly :: CPSPolyType -> [String]
ftvPoly (Forall vars t) = filter (`notElem` vars) (ftvMono t)

-- | Compute FTV of a context.
--   1. Collect FTV of all polytypes in the context
ftvContext :: Context -> [String]
ftvContext ctx = nub $ concatMap ftvPoly (Map.elems ctx)

-- | Apply substitution to a monotype recursively.
--   1. Replace variables using substitution `s`
--   2. Recurse into `TNeg` components
applySubst :: Substitution -> CPSMonoType -> CPSMonoType
applySubst s (TVar v)    = case Map.lookup v s of
                             Just t  -> t
                             Nothing -> TVar v
applySubst _ TInt        = TInt
applySubst s (TNeg ts)   = TNeg (map (applySubst s) ts)

-- | Apply substitution to a polytype
applySubstToPoly :: Substitution -> CPSPolyType -> CPSPolyType
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
occursCheck :: String -> CPSMonoType -> Bool
occursCheck u (TVar v)    = u == v
occursCheck _ TInt        = False
occursCheck u (TNeg ts)   = any (occursCheck u) ts

-- | Bind a type variable to a monotype (with occurs-check).
--   1. If `u ≡ t`, return empty substitution
--   2. Check if `u` occurs in `t` (prevents cyclic types)
--   3. Return substitution `[u ↦ t]` if safe
varBind :: String -> CPSMonoType -> TI Substitution
varBind u t
  | t == TVar u       = return Map.empty
  | occursCheck u t   = throwError $ OccursCheck u t
  | otherwise         = return (Map.singleton u t)

-- | Unify two monotypes to find the most general unifier (substitution).
--   1. If either type is a variable, bind it to the other type (with occurs-check)
--   2. Unify `TInt` with `TInt` (no substitution)
--   3. For negations `TNeg`, check arity matches, then unify component-wise
--   4. Throw `Mismatch` for all other cases
mgu :: CPSMonoType -> CPSMonoType -> TI Substitution
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt  = return Map.empty
mgu (TNeg ts1) (TNeg ts2)
  | length ts1 == length ts2 = mguLists ts1 ts2
  | otherwise = throwError $ ArityMismatch ts1 ts2
mgu t1 t2 = throwError $ Mismatch t1 t2

-- | Unify lists of types component-wise
mguLists :: [CPSMonoType] -> [CPSMonoType] -> TI Substitution
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
instantiate :: CPSPolyType -> TI CPSMonoType
instantiate (Forall vars t) = do
  freshVars <- mapM (const freshTVar) vars
  let s = Map.fromList (zip vars freshVars)
  return (applySubst s t)

-- | Generalize a monotype to a polytype by universally quantifying free variables.
--   1. Compute free type variables (FTV) of the monotype `t`
--   2. Subtract FTV of the current context `ctx` (to avoid capturing bound vars)
--   3. Quantify the remaining FTVs as `∀α.τ`
generalize :: Context -> CPSMonoType -> CPSPolyType
generalize ctx t = Forall vars t
  where
    ctxFtvs = ftvContext ctx
    vars = nub $ filter (`notElem` ctxFtvs) (ftvMono t)

-- | Infer type for an atomic term (variable/number).
--   1. If `x` is a number, return `TInt`
--   2. If `x` is a variable:
--      a. Look up its polytype in the context
--      b. Instantiate it to a fresh monotype
--   3. Return inferred monotype
inferAtom :: Context -> String -> TI CPSMonoType
inferAtom ctx x =
  case readMaybe x :: Maybe Integer of
    Just _  -> return TInt
    Nothing -> case Map.lookup x ctx of
                 Just poly -> do
                   t <- instantiate poly
                   return t
                 Nothing -> throwError $ UnboundVariable x ctx

-- | Extend context with parameters and their types
extendContextWithParams :: Context -> [Id] -> [CPSMonoType] -> Context
extendContextWithParams ctx ys paramTypes =
  foldl (\acc (p, τ) -> Map.insert p (Forall [] τ) acc) ctx (zip ys paramTypes)

-- | Infer types for a CPS command (Jump: `k(a₁,...,aₙ`).
--   1. Infer type `t1` for continuation `k`
--   2. Infer `¬[τ₁,...,τₙ]` (where `τᵢ` are arg types)
--   3. Unify `t1` with `¬[τ₁,...,τₙ]`
--   4. Return substitution
inferCommand :: Context -> Command -> TI Substitution
inferCommand ctx (Jump k xs) = do
  t1 <- inferAtom ctx k
  t2 <- mapM (inferAtom ctx) xs
  s <- mgu t1 (TNeg t2)
  return s

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
inferWithCtx :: Command -> TI CPSPolyType
inferWithCtx cmd = do
  initialType <- freshTVar
  let ctx = Map.singleton initialCont (Forall [] initialType)

  subst <- inferCommand ctx cmd
  let finalCtx = applySubstToContext subst ctx

  case Map.lookup initialCont finalCtx of
    Just (Forall _ monoType) ->
      let generalized = generalize (Map.delete initialCont finalCtx) monoType
          normalized = normalizePolyType generalized
      in return normalized
    Nothing -> throwError (UnboundVariable initialCont ctx)

-- | Normalize quantified variables to α, β, γ, etc.
normalizePolyType :: CPSPolyType -> CPSPolyType
normalizePolyType (Forall vars t) =
  let newVars = take (length vars) greekLetters
      subst   = zip vars newVars
      t'      = applySubstToMono subst t
  in Forall newVars t'

applySubstToMono :: [(String, String)] -> CPSMonoType -> CPSMonoType
applySubstToMono subst = go
  where
    go (TVar v)     = TVar $ Data.Maybe.fromMaybe v (lookup v subst)
    go TInt         = TInt
    go (TNeg ts)    = TNeg (map go ts)

isSubtypeOfPoly :: CPSPolyType -> CPSPolyType -> Either TypeError (Maybe Substitution)
isSubtypeOfPoly (Forall vars1 t1) (Forall vars2 t2) = 
  runTI $ do
    freshVars1 <- replicateM (length vars1) freshTVar
    let subst1 = Map.fromList (zip vars1 freshVars1)
    freshVars2 <- replicateM (length vars2) freshTVar
    let subst2 = Map.fromList (zip vars2 freshVars2)
    pure $ isSubtypeOf (applySubst subst1 t1) (applySubst subst2 t2)

isSubtypeOf :: CPSMonoType -> CPSMonoType -> Maybe Substitution
isSubtypeOf t1 t2 = match t1 t2 Map.empty
  where
    match :: CPSMonoType -> CPSMonoType -> Substitution -> Maybe Substitution
    match (TVar a) t subst =
        case Map.lookup a subst of
            Just tExisting -> if tExisting == t then Just subst else Nothing
            Nothing ->
                if occursCheck a t
                    then Nothing
                    else Just (Map.insert a t subst)
    match TInt TInt subst = Just subst
    match (TNeg ts1) (TNeg ts2) subst
        | length ts1 == length ts2 = foldM (\s (t1', t2') -> match t1' t2' s) subst (zip ts1 ts2)
        | otherwise = Nothing
    match _ _ _ = Nothing
