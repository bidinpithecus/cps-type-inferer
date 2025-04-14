module CPS.Typing where

import Data.List (intercalate)
import qualified Data.Map as Map
import Lambda.Typing ( Id )

-- | CPS Commands:
--   * Jump to continuation with arguments: k⟨x₁, ..., xₙ⟩
--   * Bind continuation: let k⟨x₁, ..., xₙ⟩ = c in b
data Command
  = Jump Id [Id]
  | Bind Command Id [Id] Command
  deriving (Eq)

instance Show Command where
  show = showThielecke

-- | Appel-style notation: k⟨x₁, ..., xₙ⟩ and b { k⟨y₁, ..., yₙ⟩ = c }
showAppel :: Command -> String
showAppel (Jump k xs) = k ++ "<" ++ intercalate ", " xs ++ ">"
showAppel (Bind b y ys c) =
  let body = case b of
        Bind {} -> "(" ++ showAppel b ++ ")"
        _ -> showAppel b
   in body ++ " { " ++ y ++ "<" ++ intercalate ", " ys ++ "> = " ++ showAppel c ++ "}"

-- | Thielecke-style notation: k(x₁, ..., xₙ) and let k(y₁, ..., yₙ) = c in b
showThielecke :: Command -> String
showThielecke (Jump k xs) = k ++ "(" ++ intercalate ", " xs ++ ")"
showThielecke (Bind b y ys c) =
  let body = case b of
        Bind {} -> "(" ++ showThielecke b ++ ")"
        _ -> showThielecke b
   in "let " ++ y ++ "(" ++ intercalate ", " ys ++ ") = " ++ showThielecke c ++ " in " ++ body

-- | CPS Monotypes:
--   * Type variables (α), integers (int), negation types (¬[τ])
data MonoType
  = TVar Id
  | TInt
  | TNeg [MonoType]
  deriving (Eq)

-- | CPS Polytypes: Universally quantified types (∀α.τ)
data PolyType
  = Forall [Id] MonoType
  deriving (Eq)

-- | Typing context mapping variables to polytypes
type Context = Map.Map Id PolyType

-- | Substitution mapping type variables to monotypes
type Substitution = Map.Map Id MonoType

instance Show PolyType where
  show (Forall vars t)
    | null vars = show t
    | otherwise = "∀" ++ intercalate "," vars ++ ". " ++ show t

instance Show MonoType where
  show (TVar v) = v
  show TInt = "int"
  show (TNeg [t]) = "¬" ++ show t
  show (TNeg ts) = "¬(" ++ intercalate ", " (map show ts) ++ ")"

-- | Pretty-print substitutions for debugging
prettySubst :: Substitution -> String
prettySubst s = "{ " ++ intercalate ", " (map (\(k,v) -> k ++ " ↦ " ++ show v) $ Map.toList s) ++ " }"
