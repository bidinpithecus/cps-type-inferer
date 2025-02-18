module CPS.Translation where

import CPS.Typing
import Control.Monad.State (evalState)
import Lambda.Typing

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

callByNameToCps :: Expr -> Command
callByNameToCps expr = evalState (callByName expr "k") (0, 0)

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

callByValueToCps :: Expr -> Command
callByValueToCps expr = evalState (callByValue expr "k") (0, 0)
