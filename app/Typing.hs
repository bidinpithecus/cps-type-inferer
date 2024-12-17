module Typing where

import Control.Monad.State (State)
import Data.List (intercalate)

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

-- ADT representing CPS terms
-- k<x> -- That is, a call to `k` with `x` as arguments
-- b { k<y> = c } -- That is, defining `k` in `b` as a continuation `c`, with `y` as parameters
data CPS
  = Jump Id [Id]
  | Bind CPS Id [Id] CPS
  deriving (Eq)

instance Show CPS where
  show (Jump k xs) = k ++ "<" ++ intercalate ", " xs ++ ">"
  show (Bind b y ys c) =
    let body = case b of
          Bind {} -> "(" ++ show b ++ ")"
          _ -> show b
     in body ++ " { " ++ y ++ "<" ++ intercalate ", " ys ++ "> = " ++ show c ++ "}"

-- Fresh variables
type FreshM a = State Int a