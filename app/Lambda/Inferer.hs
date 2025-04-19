{-# LANGUAGE LambdaCase #-}
module Lambda.Inferer where

import Lambda.Typing
    (
      SimpleType(..),
      TI,
      Assump(..),
      Expr(..),
      freshTVar,
      Subst,
      Subs(apply, tv),
      runTI,
      (-->),
      (@@),
      (/+/) )
import Utils.Typing (Id, greekLetters)
import Data.Maybe (fromMaybe)

varBind :: Id -> SimpleType -> Maybe Subst
varBind u t | t == TVar u   = Just []
            | u `elem` tv t = Nothing
            | otherwise     = Just [(u, t)]

mgu :: (SimpleType, SimpleType) -> Maybe [(Id, SimpleType)]
mgu (TArr l r,  TArr l' r') = do s1 <- mgu (l,l')
                                 s2 <- mgu (apply s1 r ,  apply s1 r')
                                 return (s2 @@ s1)
mgu (t,        TVar u   )   =  varBind u t
mgu (TVar u,   t        )   =  varBind u t

unify :: SimpleType -> SimpleType -> [(Id, SimpleType)]
unify t t' =  case mgu (t,t') of
    Nothing -> error ("\ntrying to unify:\n" ++ show t ++ "\nand\n" ++
                      show t'++"\n")
    Just s  -> s

tiContext :: [Assump] -> Id -> SimpleType
tiContext g i = if l /= [] then t else error ("Undefined: " ++ i ++ "\n")
   where
      l = dropWhile (\(i' :>: _) -> i /= i' ) g
      (_ :>: t) = head l

tiExpr :: [Assump] -> Expr -> TI (SimpleType, Subst)
tiExpr g (Var i) = return (tiContext g i, [])
tiExpr g (App e e') = do (t, s1) <- tiExpr g e
                         (t', s2) <- tiExpr (apply s1 g) e'
                         b <- freshTVar
                         let s3 = unify (apply s2 t) (t' --> b)
                         return (apply s3 b, s3 @@ s2 @@ s1)
tiExpr g (Lam i e) = do b <- freshTVar
                        (t, s)  <- tiExpr (g/+/[i:>:b]) e
                        return (apply s (b --> t), s)

inferExpr :: Expr -> (SimpleType, Subst)
inferExpr e = 
  let (t, subst) = runTI (tiExpr [] e)
      normalizedType = normalizeSimpleType t
  in (normalizedType, subst)

-- | Normalize type variables in a SimpleType
normalizeSimpleType :: SimpleType -> SimpleType
normalizeSimpleType t =
  let vars = collectUniqueVars t
      subst = zip vars (take (length vars) greekLetters)
  in applySubstToSimple subst t
  where
    collectUniqueVars :: SimpleType -> [Id]
    collectUniqueVars typ = go typ []
      where
        go :: SimpleType -> [Id] -> [Id]
        go (TVar v) acc
          | v `elem` acc = acc
          | otherwise    = acc ++ [v]
        go (TArr t1 t2) acc = 
          let acc' = go t1 acc
          in go t2 acc'

    applySubstToSimple :: [(Id, Id)] -> SimpleType -> SimpleType
    applySubstToSimple subst = \case
      TVar v    -> TVar (fromMaybe v (lookup v subst))
      TArr t1 t2 -> TArr (applySubstToSimple subst t1) (applySubstToSimple subst t2)
