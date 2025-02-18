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
  show = showThielecke

-- k⟨ ⃗x⟩
-- b { k⟨ ⃗y⟩ c }
showAppel :: Command -> String
showAppel (Jump k xs) = k ++ "<" ++ intercalate ", " xs ++ ">"
showAppel (Bind b y ys c) =
  let body = case b of
        Bind {} -> "(" ++ showAppel b ++ ")"
        _ -> showAppel b
   in body ++ " { " ++ y ++ "<" ++ intercalate ", " ys ++ "> = " ++ showAppel c ++ "}"

-- k( ⃗x)
-- let k( ⃗y) = c in b
showThielecke :: Command -> String
showThielecke (Jump k xs) = k ++ "(" ++ intercalate ", " xs ++ ")"
showThielecke (Bind b y ys c) =
  let body = case b of
        Bind {} -> "(" ++ showThielecke b ++ ")"
        _ -> showThielecke b
   in "let " ++ y ++ "(" ++ intercalate ", " ys ++ ") = " ++ showThielecke c ++ " in " ++ body

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
