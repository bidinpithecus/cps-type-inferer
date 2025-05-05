{-# LANGUAGE LambdaCase #-}
module CPS.Translation where

import CPS.Typing
    ( initialCont, Command(..), CPSMonoType(TNeg, TVar), CPSPolyType(..) )
import Control.Monad.State ( modify, evalState, runState, State )
import Lambda.Typing ( LambdaMonoType(..), Expr(..) )
import qualified Data.Set as S
import Utils.Typing ( Id, FreshM, freshVar, freshCont )

-- Plotkin's call-by-name translation:
-- [[x]]   = x(k)
-- [[λx.e]] = k(v) { v(x, k') = [[e]] }
-- [[f e]] = [[f]] { k(f) = f(v, k') { k'(v) = [[e]] } }
-- [[let x = b in a]] = [[a]] { x(k) = [[b]] }
callByName :: Expr -> Id -> FreshM Command
callByName (Var x) k = return $ Jump x [k]
callByName (Lam x e) k = do
  k' <- freshCont
  v <- freshVar
  let jump = Jump k [v]
  eCall <- callByName e k'
  return $ Bind jump v [x, k'] eCall
callByName (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argCont <- freshCont
  argVar <- freshVar

  fCall <- callByName f fCont
  let appCmd = Jump fVar [argVar, k]
  eCall <- callByName e argCont
  let innerBind = Bind appCmd argVar [argCont] eCall
  return $ Bind fCall fCont [fVar] innerBind
callByName (Let x b a) k = do
  k' <- freshCont
  bCall <- callByName b k'
  aCall <- callByName a k
  return $ Bind aCall x [k'] bCall

cbnExprTranslation :: Expr -> Command
cbnExprTranslation expr = evalState (callByName expr initialCont) (0, 0)

-- Plotkin's call-by-value translation:
-- [[x]]   = k(x)
-- [[λx.e]] = k(v) { v(x, k') = [[e]] }
-- [[f e]] = [[f]] { k(f) = [[e]] { k'(v) = f(v, k) } }
-- [[let x = b in a]] = [[b]] { k(x) = [[a]] }
callByValue :: Expr -> Id -> FreshM Command
callByValue (Var x) k = return $ Jump k [x]
callByValue (Lam x e) k = do
  k' <- freshCont
  v <- freshVar
  let bindBody = Jump k [v]
  body <- callByValue e k'
  return $ Bind bindBody v [x, k'] body
callByValue (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argCont <- freshCont
  argVar <- freshVar

  fCall <- callByValue f fCont
  eCall <- callByValue e argCont
  let appCmd = Jump fVar [argVar, k]
  let argBind = Bind eCall argCont [argVar] appCmd
  return $ Bind fCall fCont [fVar] argBind
callByValue (Let x b a) k = do
  k' <- freshCont
  bCall <- callByValue b k'
  aCall <- callByValue a k
  return $ Bind bCall k' [x] aCall

cbvExprTranslation :: Expr -> Command
cbvExprTranslation expr = evalState (callByValue expr initialCont) (0, 0)

-- | Call-by-Name CPS type translation
cbnTypeTranslation :: LambdaMonoType -> CPSPolyType
cbnTypeTranslation st =
  let (translatedMono, freeVars) = runState (cbnTranslate st) S.empty
  in Forall (S.toList freeVars) translatedMono
  where
    cbnTranslate :: LambdaMonoType -> State (S.Set String) CPSMonoType
    cbnTranslate = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ TNeg [CPS.Typing.TVar varId]
      TArr a b -> do
        a' <- cbnTranslate a
        b' <- cbnTranslate b
        return $ TNeg [TNeg [TNeg [a'], b']]

-- | Call-by-Value CPS type translation
cbvTypeTranslation :: LambdaMonoType -> CPSPolyType
cbvTypeTranslation st =
  let (translatedMono, freeVars) = runState (cbvTranslate st) S.empty
      finalMono = TNeg [translatedMono]
  in Forall (S.toList freeVars) finalMono
  where
    cbvTranslate :: LambdaMonoType -> State (S.Set String) CPSMonoType
    cbvTranslate = \case
      Lambda.Typing.TVar varId -> do
        modify (S.insert varId)
        return $ CPS.Typing.TVar varId
      TArr a b -> do
        a' <- cbvTranslate a
        b' <- cbvTranslate b
        return $ TNeg [a', TNeg [b']]
