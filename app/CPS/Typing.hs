module CPS.Typing where

import Data.List (intercalate)
import qualified Data.Map as Map
import Lambda.Typing

-- ADT representing CPS terms
-- k<x> -- That is, a call to `k` with `x` as arguments
-- b { k<y> = c } -- That is, defining `k` in `b` as a continuation `c`, with `y` as parameters
data Command
  = Jump Id [Id]
  | Bind Command Id [Id] Command
  deriving (Eq)

instance Show Command where
  show (Jump k xs) = k ++ "<" ++ intercalate ", " xs ++ ">"
  show (Bind b y ys c) =
    let body = case b of
          Bind {} -> "(" ++ show b ++ ")"
          _ -> show b
     in body ++ " { " ++ y ++ "<" ++ intercalate ", " ys ++ "> = " ++ show c ++ "}"

-- ADT represeting CPS Monotype in the Polymorphic Type system types
-- TVar String -- That is, a type variable (α)
-- TInt -- That is, the integer type (int)
-- TNeg [MonoType] -- That is, the negation type (¬τ)
data MonoType
  = TVar String
  | TInt
  | TNeg [MonoType]
  deriving (Eq)

-- ADT represeting CPS Polytype in the Polymorphic Type system types
-- Forall [String] MonoType -- That is, the universally quantified type (∀ α. τ)
data PolyType
  = Forall [String] MonoType
  deriving (Eq)

-- ADT represeting CPS Context in the Polymorphic Type system types
type Context =
  Map.Map String PolyType

type Substitution =
  Map.Map String MonoType
