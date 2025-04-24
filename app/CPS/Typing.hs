module CPS.Typing where

import Data.List (intercalate)
import qualified Data.Map as Map
import Utils.Typing ( Id )

-- | CPS Commands:
--   * Jump to continuation with arguments: k⟨x₁, ..., xₙ⟩
--   * Bind continuation: let k⟨x₁, ..., xₙ⟩ = c in b
data Command
  = Jump Id [Id]
  | Bind Command Id [Id] Command
  deriving (Eq)

instance Show Command where
  show = showThielecke

initialCont :: Id
initialCont = "k"

-- | Appel-style notation: k⟨x₁, ..., xₙ⟩ and b { k⟨y₁, ..., yₙ⟩ = c }
showAppel :: Command -> String
showAppel cmd = showAppel' cmd 0

showAppel' :: Command -> Int -> String
showAppel' (Jump k xs) indent = replicate indent ' ' ++ k ++ "<" ++ intercalate ", " xs ++ ">"
showAppel' (Bind b y ys c) indent =
  let body = case b of
        Bind {} -> 
          let bodyStr = showAppel' b (indent + 4)
          in replicate indent ' ' ++ "(\n" ++ bodyStr ++ "\n" ++ replicate indent ' ' ++ ")"
        _ -> showAppel' b indent
      defLine = replicate (indent + 4) ' ' ++ y ++ "<" ++ intercalate ", " ys ++ "> = " ++ showAppel' c (indent + 4)
  in body ++ " {\n" ++ defLine ++ "\n" ++ replicate indent ' ' ++ "}"

-- | Thielecke-style notation: k(x₁, ..., xₙ) and let k(y₁, ..., yₙ) = c in b
showThielecke :: Command -> String
showThielecke cmd = showThielecke' cmd 0

showThielecke' :: Command -> Int -> String
showThielecke' (Jump k xs) indent = replicate indent ' ' ++ k ++ "(" ++ intercalate ", " xs ++ ")"
showThielecke' (Bind b y ys c) indent =
  let letLine = replicate indent ' ' ++ "let " ++ y ++ "(" ++ intercalate ", " ys ++ ") ="
      cStr = showThielecke' c (indent + 4)
      inLine = replicate indent ' ' ++ "in"
      bodyStr = showThielecke' b (indent + 4)
  in intercalate "\n" [letLine, cStr, inLine, bodyStr]

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
