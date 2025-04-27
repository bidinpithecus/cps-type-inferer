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
showAppel cmd = showAppel' cmd 0 6

showAppel' :: Command -> Int -> Int -> String
showAppel' (Jump k xs) indentLevel _ = replicate indentLevel ' ' ++ k ++ "⟨" ++ intercalate ", " xs ++ "⟩"
showAppel' (Bind b y ys c) indentLevel indentQtd =
  let body = case b of
        Bind {} -> 
          let bodyStr = showAppel' b (indentLevel + indentQtd) indentQtd
          in replicate indentLevel ' ' ++ "(\n" ++ bodyStr ++ "\n" ++ replicate indentLevel ' ' ++ "⟩"
        _ -> showAppel' b indentLevel indentQtd
      defLine = replicate (indentLevel + indentQtd) ' ' ++ y ++ "⟨" ++ intercalate ", " ys ++ "⟩ = " ++ showAppel' c (indentLevel + indentQtd) indentQtd
  in body ++ " {\n" ++ defLine ++ "\n" ++ replicate indentLevel ' ' ++ "}"

-- | Thielecke-style notation: k(x₁, ..., xₙ) and let k(y₁, ..., yₙ) = c in b
showThielecke :: Command -> String
showThielecke cmd = showThielecke' cmd 0 6

showThielecke' :: Command -> Int -> Int -> String
showThielecke' (Jump k xs) indentLevel _ = replicate indentLevel ' ' ++ k ++ "(" ++ intercalate ", " xs ++ ")"
showThielecke' (Bind b y ys c) indentLevel indentQtd =
  let letLine = replicate indentLevel ' ' ++ "let " ++ y ++ "(" ++ intercalate ", " ys ++ ") ="
      cStr = showThielecke' c (indentLevel + indentQtd) indentQtd
      inLine = replicate indentLevel ' ' ++ "in"
      bodyStr = showThielecke' b (indentLevel + indentQtd) indentQtd
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
