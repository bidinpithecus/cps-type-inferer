module Lambda.Typing where

import Control.Monad.State (MonadState (get, put), State)

type Id = String

-- ADT representing Lambda expressions
-- x
-- \x. e
-- e e
data Expr
  = Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  show (Var var) = var
  show (Lam var expr) = "λ" ++ var ++ ". " ++ show expr
  show (App expr1 expr2) =
    let e1 = case expr1 of
          Lam _ _ -> "(" ++ show expr1 ++ ")"
          _ -> show expr1
        e2 = case expr2 of
          App _ _ -> "(" ++ show expr2 ++ ")"
          Lam _ _ -> "(" ++ show expr2 ++ ")"
          _ -> show expr2
     in e1 ++ " " ++ e2

-- Fresh variables
type FreshM a = State (Int, Int) a

freshVar :: FreshM Id
freshVar = do
  (vCount, kCount) <- get
  put (vCount + 1, kCount)
  return $ "v" ++ show vCount

freshCont :: FreshM Id
freshCont = do
  (vCount, kCount) <- get
  put (vCount, kCount + 1)
  return $ "k" ++ show kCount
