module Typing where

import Control.Monad.State

type Id = String

-- ADT representing Lambda expressions
-- x
-- \x. e
-- e e
data Expr
  = Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving (Eq, Show)

-- ADT representing CPS terms
-- k<x> -- That is, a call to `k` with `x` as arguments
-- b { k<y> = c } -- That is, defining `k` in `b` as a continuation `c`, with `y` as parameters
data CPS
  = Jump Id [Id]
  | Bind CPS Id [Id] CPS
  deriving (Eq, Show)

-- Fresh variables
type FreshM a = State Int a