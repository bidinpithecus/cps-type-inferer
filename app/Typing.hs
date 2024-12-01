module Typing where

type Id = String

-- ADT representing Lambda expressions
data Expr
  = Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving (Eq, Show)
