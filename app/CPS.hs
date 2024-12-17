module CPS where

import Control.Monad.State (MonadState (get, put), evalState)
import Typing (CPS (..), Expr (..), FreshM, Id)

freshVar :: FreshM Id
freshVar = do
  n <- get
  put (n + 1)
  return $ "v" ++ show n

-- Plotkin's call-by-name translation:
-- [[x]] = x<k>
-- [[\x.e]] = k<v> { v<x, k> = [[e]] }
-- [[f e]] = [[f]] { k<f> = f<v, k> { k<v> = [[e]] } }
callByNameTranslation :: Expr -> Id -> FreshM CPS
callByNameTranslation (Var x) k = return $ Jump x [k] -- x<k>
callByNameTranslation (Lam x e) k = do
  v <- freshVar -- v
  let b = Jump k [v] -- k<v>
  c <- callByNameTranslation e k -- [[e]]
  return $ Bind b v [x, k] c -- k<v> { v<x, k> = [[e]] }
callByNameTranslation (App f e) k = do
  v1 <- freshVar -- f
  v2 <- freshVar -- v
  f' <- callByNameTranslation f k -- [[f]]
  e' <- callByNameTranslation e k -- [[e]]
  let c = Jump v1 [v2, k] -- f<v, k>
  let innerBind = Bind c k [v2] e' -- c { k<v> = [[e]] }
  return $ Bind f' k [v1] innerBind -- [[f]] { k<f> = f<v, k> { k<v> = [[e]] } }

callByNameToCps :: Expr -> CPS
callByNameToCps expr = evalState (callByNameTranslation expr "k") 0

-- Plotkin's call-by-value translation:
-- [[x]] = k<x>
-- [[\x.e]] = k<v> { v<x, k> = [[e]] }
-- [[f e]] = [[f]] { k<f> = [[e]] { k<v> = f<v, k> } }
callByValueTranslation :: Expr -> Id -> FreshM CPS
callByValueTranslation (Var x) k = return $ Jump k [x] -- k<x>
callByValueTranslation (Lam x e) k = do
  v <- freshVar -- v
  let b = Jump k [v] -- k<v>
  c <- callByValueTranslation e k -- [[e]]
  return $ Bind b v [x, k] c -- k<v> { v<x, k> = [[e]] }
callByValueTranslation (App f e) k = do
  v1 <- freshVar -- f
  v2 <- freshVar -- v
  f' <- callByValueTranslation f k -- [[f]]
  e' <- callByValueTranslation e k -- [[e]]
  let c = Jump v1 [v2, k] -- f<v, k>
  let innerBind = Bind e' k [v2] c -- [[e]] { k<v> = c }
  return $ Bind f' k [v1] innerBind -- [[f]] { k<f> = [[e]] { k<v> = f<v, k> } }

callByValueToCps :: Expr -> CPS
callByValueToCps expr = evalState (callByValueTranslation expr "k") 0
