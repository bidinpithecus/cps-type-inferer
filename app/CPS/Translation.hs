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
callByName (Var x) k = return $ Jump x [k]  -- x(k)
callByName (Lam x e) k = do
  v <- freshVar  -- Fresh variable for the lambda abstraction
  k' <- freshCont  -- Fresh continuation for the body
  let bindBody = Jump k [v]  -- k(v)
  body <- callByName e k'  -- [[e]] under k'
  return $ Bind bindBody v [x, k'] body  -- k(v) { v(x, k') = [[e]] }
callByName (App f e) k = do
  funcCont <- freshCont  -- Continuation to receive the function
  funcVar <- freshVar  -- Variable to hold the function from f
  argVar <- freshVar  -- Argument variable for the application
  argCont <- freshCont  -- Continuation for the argument e

  fTrans <- callByName f funcCont  -- [[f]] with continuation funcCont
  let appCmd = Jump funcVar [argVar, argCont]
  eTrans <- callByName e argCont  -- [[e]] with continuation argCont
  let innerBind = Bind appCmd argCont [argVar] eTrans
  let outerBind = Bind fTrans k [funcVar] innerBind
  return outerBind  -- [[f]] { k(funcVar) = ... }

cbnExprTranslation :: Expr -> Command
cbnExprTranslation expr = evalState (callByName expr initialCont) (0, 0)

-- Plotkin's call-by-value translation:
-- [[x]]   = k(x)
-- [[λx.e]] = k(v) { v(x, k') = [[e]] }
-- [[f e]] = [[f]] { k(f) = [[e]] { k'(v) = f(v, k) } }
callByValue :: Expr -> Id -> FreshM Command
callByValue (Var x) k = return $ Jump k [x]  -- k(x)
callByValue (Lam x e) k = do
  v <- freshVar  -- Fresh variable for the lambda abstraction
  k' <- freshCont  -- Fresh continuation for the body
  let bindBody = Jump k [v]  -- k(v)
  body <- callByValue e k'  -- [[e]] under k'
  return $ Bind bindBody v [x, k'] body  -- k(v) { v(x,k')=[[e]] }
callByValue (App f e) k = do
  funcVar <- freshVar  -- Variable to hold the function result (v1)
  funcCont <- freshCont  -- Continuation for the function (k1)
  argVar <- freshVar  -- Variable to hold the argument result (v2)
  argCont <- freshCont  -- Continuation for the argument (k2)

  fTrans <- callByValue f funcCont
  eTrans <- callByValue e argCont
  let appCmd = Jump funcVar [argVar, k]
  let argBind = Bind appCmd argCont [argVar] eTrans
  let funcBind = Bind fTrans funcCont [funcVar] argBind
  return funcBind  -- [[f]] { k1(funcVar) = [[e]] { k2(argVar) = funcVar(argVar,k) } }

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
