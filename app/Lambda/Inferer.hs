{-# LANGUAGE LambdaCase #-}
module Lambda.Inferer where

import Lambda.Typing
    (
      LambdaMonoType(..),
      TI,
      Assump(..),
      Expr(..),
      freshTVar,
      Subst,
      Subs(apply, tv),
      runTI,
      (-->),
      (@@),
      (/+/), LambdaPolyType (..))
import Utils.Typing (Id, greekLetters)
import Data.Maybe (fromMaybe)

-- In Inferer.hs
generalize :: [Assump] -> LambdaMonoType -> LambdaPolyType
generalize ctx t =
  let ctxFvs = concatMap (\(_ :>: pt) -> tv pt) ctx
      fvs = tv t
      quantified = filter (`notElem` ctxFvs) fvs
  in Forall quantified t

instantiate :: LambdaPolyType -> TI LambdaMonoType
instantiate (Forall vs t) = do
  freshVars <- mapM (const freshTVar) vs
  let s = zip vs freshVars
  return $ apply s t

varBind :: Id -> LambdaMonoType -> Maybe Subst
varBind u t | t == TVar u   = Just []
            | u `elem` tv t = Nothing
            | otherwise     = Just [(u, t)]

mgu :: (LambdaMonoType, LambdaMonoType) -> Maybe [(Id, LambdaMonoType)]
mgu (TArr l r,  TArr l' r') = do s1 <- mgu (l,l')
                                 s2 <- mgu (apply s1 r ,  apply s1 r')
                                 return (s2 @@ s1)
mgu (t,        TVar u   )   =  varBind u t
mgu (TVar u,   t        )   =  varBind u t

unify :: LambdaMonoType -> LambdaMonoType -> [(Id, LambdaMonoType)]
unify t t' =  case mgu (t,t') of
    Nothing -> error ("\ntrying to unify:\n" ++ show t ++ "\nand\n" ++
                      show t'++"\n")
    Just s  -> s

tiContext :: [Assump] -> Id -> LambdaPolyType
tiContext g i = 
  case filter (\(i' :>: _) -> i == i') g of
    [] -> error ("Undefined: " ++ i)
    ((_ :>: pt) : _) -> pt

tiExpr :: [Assump] -> Expr -> TI (LambdaMonoType, Subst)
tiExpr g (Var i) = do
  let pt = tiContext g i
  monoT <- instantiate pt
  return (monoT, [])

tiExpr g (Lam i e) = do
  b <- freshTVar
  let pt = Forall [] b
  (t, s) <- tiExpr (g /+/ [i :>: pt]) e
  return (apply s (b --> t), s)

tiExpr g (App e e') = do 
  (t, s1) <- tiExpr g e
  (t', s2) <- tiExpr (apply s1 g) e'
  b <- freshTVar
  let s3 = unify (apply s2 t) (t' --> b)
  return (apply s3 b, s3 @@ s2 @@ s1)

tiExpr g (Let i e1 e2) = do
  (t1, s1) <- tiExpr g e1
  let t1' = apply s1 t1
      pt = generalize (apply s1 g) t1'  -- Generalize the type
  (t2, s2) <- tiExpr (apply s1 g /+/ [i :>: pt]) e2  -- Extend context
  return (t2, s2 @@ s1)

inferExpr :: Expr -> (LambdaMonoType, Subst)
inferExpr e = 
  let (t, subst) = runTI (tiExpr [] e)
      normalizedType = normalizeSimpleType t
  in (normalizedType, subst)

-- | Normalize type variables in a LambdaMonoType
normalizeSimpleType :: LambdaMonoType -> LambdaMonoType
normalizeSimpleType t =
  let vars = collectUniqueVars t
      subst = zip vars (take (length vars) greekLetters)
  in applySubstToSimple subst t
  where
    collectUniqueVars :: LambdaMonoType -> [Id]
    collectUniqueVars typ = go typ []
      where
        go :: LambdaMonoType -> [Id] -> [Id]
        go (TVar v) acc
          | v `elem` acc = acc
          | otherwise    = acc ++ [v]
        go (TArr t1 t2) acc = 
          let acc' = go t1 acc
          in go t2 acc'

    applySubstToSimple :: [(Id, Id)] -> LambdaMonoType -> LambdaMonoType
    applySubstToSimple subst = \case
      TVar v    -> TVar (fromMaybe v (lookup v subst))
      TArr t1 t2 -> TArr (applySubstToSimple subst t1) (applySubstToSimple subst t2)
