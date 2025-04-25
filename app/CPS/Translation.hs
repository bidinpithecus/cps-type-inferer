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
-- [[x]]   = x(k)
-- [[λx.e]] = k(v) { v(x, k') = [[e]] }
-- [[f e]] = [[f]] { k(f) = f(v, k') { k'(v) = [[e]] } }
callByName :: Expr -> Id -> FreshM Command
callByName (Var x) k = return $ Jump x [k]
callByName (Lam x e) k = do
  v <- freshVar
  k' <- freshCont
  let jump = Jump k [v]
  eCall <- callByName e k'
  return $ Bind jump v [x, k'] eCall
callByName (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argVar <- freshVar
  argCont <- freshCont

  fCall <- callByName f fCont
  let appCmd = Jump fVar [argVar, k]
  eCall <- callByName e argCont
  let innerBind = Bind appCmd argVar [argCont] eCall
  return $ Bind fCall fCont [fVar] innerBind

cbnExprTranslation :: Expr -> Command
cbnExprTranslation expr = evalState (callByName expr initialCont) (0, 0)

-- Plotkin's call-by-value translation:
-- [[x]]   = k(x)
-- [[λx.e]] = k(v) { v(x, k') = [[e]] }
-- [[f e]] = [[f]] { k(f) = [[e]] { k'(v) = f(v, k) } }
callByValue :: Expr -> Id -> FreshM Command
callByValue (Var x) k = return $ Jump k [x]
callByValue (Lam x e) k = do
  v <- freshVar
  k' <- freshCont
  let bindBody = Jump k [v]
  body <- callByValue e k'
  return $ Bind bindBody v [x, k'] body
callByValue (App f e) k = do
  fCont <- freshCont
  fVar <- freshVar
  argVar <- freshVar
  argCont <- freshCont

  fCall <- callByValue f fCont
  eCall <- callByValue e argCont
  let appCmd = Jump fVar [argVar, k]
  let argBind = Bind eCall argCont [argVar] appCmd
  return $ Bind fCall fCont [fVar] argBind

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
