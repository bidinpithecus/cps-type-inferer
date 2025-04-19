{-# LANGUAGE LambdaCase #-}
module CPS.Translation where

import CPS.Typing
    ( initialCont, Command(..), MonoType(TNeg, TVar), PolyType(..) )
import Control.Monad.State ( modify, evalState, runState, State )
import Lambda.Typing
    ( SimpleType(..), Expr(..), FreshM, freshVar, freshCont )
import qualified Data.Set as S
import Utils.Typing (Id)

-- Plotkin's call-by-name translation:
-- [[x]] = x<k>
-- [[\x.e]] = k<v> { v<x, k> = [[e]] }
-- [[f e]] = [[f]] { k<f> = f<v, k> { k<v> = [[e]] } }
callByName :: Expr -> Id -> FreshM Command
callByName (Var x) k = return $ Jump x [k] -- x<k>
callByName (Lam x e) k = do
  v <- freshVar -- v
  k' <- freshCont
  let b = Jump k [v] -- k<v>
  c <- callByName e k' -- [[e]]
  return $ Bind b v [x, k'] c -- k<v> { v<x, k'> = [[e]] }
callByName (App f e) k = do
  v1 <- freshVar -- f
  v2 <- freshVar -- v
  k' <- freshCont
  f' <- callByName f k -- [[f]]
  e' <- callByName e k' -- [[e]]
  let c = Jump v1 [v2, k] -- f<v, k>
  let innerBind = Bind c k' [v2] e' -- c { k'<v> = [[e]] }
  return $ Bind f' k [v1] innerBind -- [[f]] { k<f> = f<v, k> { k<v> = [[e]] } }

cbnExprTranslation :: Expr -> Command
cbnExprTranslation expr = evalState (callByName expr initialCont) (0, 0)

-- Plotkin's call-by-value translation:
-- [[x]] = k<x>
-- [[\x.e]] = k<v> { v<x, k> = [[e]] }
-- [[f e]] = [[f]] { k<f> = [[e]] { k<v> = f<v, k> } }
callByValue :: Expr -> Id -> FreshM Command
callByValue (Var x) k = do return $ Jump k [x] -- k<x>
callByValue (Lam x e) k = do
  v <- freshVar -- v
  k' <- freshCont
  let b = Jump k [v] -- k<v>
  c <- callByValue e k' -- [[e]]
  return $ Bind b v [x, k'] c -- k<v> { v<x, k'> = [[e]] }
callByValue (App f e) k = do
  v1 <- freshVar -- f
  v2 <- freshVar -- v
  k' <- freshCont
  f' <- callByValue f k -- [[f]]
  e' <- callByValue e k' -- [[e]]
  let c = Jump v1 [v2, k] -- f<v, k>
  let innerBind = Bind e' k' [v2] c -- [[e]] { k'<v> = c }
  return $ Bind f' k [v1] innerBind -- [[f]] { k<f> = [[e]] { k'<v> = f<v, k> } }

cbvExprTranslation :: Expr -> Command
cbvExprTranslation expr = evalState (callByValue expr initialCont) (0, 0)

-- | Call-by-Name CPS type translation
cbnTypeTranslation :: SimpleType -> PolyType
cbnTypeTranslation st =
  let (translatedMono, freeVars) = runState (cbnTranslate st) S.empty
  in Forall (S.toList freeVars) translatedMono
  where
    cbnTranslate :: SimpleType -> State (S.Set String) MonoType
    cbnTranslate = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ TNeg [CPS.Typing.TVar varId]
      TArr a b -> do
        a' <- cbnTranslate a
        b' <- cbnTranslate b
        return $ TNeg [TNeg [TNeg [a'], b']]

-- | Call-by-Value CPS type translation
cbvTypeTranslation :: SimpleType -> PolyType
cbvTypeTranslation st =
  let (translatedMono, freeVars) = runState (cbvTranslate st) S.empty
      finalMono = TNeg [translatedMono]
  in Forall (S.toList freeVars) finalMono
  where
    cbvTranslate :: SimpleType -> State (S.Set String) MonoType
    cbvTranslate = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ CPS.Typing.TVar varId
      TArr a b -> do
        a' <- cbvTranslate a
        b' <- cbvTranslate b
        return $ TNeg [a', TNeg [b']]
