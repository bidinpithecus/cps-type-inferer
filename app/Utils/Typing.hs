module Utils.Typing where

type Id = String

greekLetters :: [String]
greekLetters = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
                "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

greekVar :: Int -> String
greekVar n =
  let (q, r) = n `divMod` length greekLetters
      suffix = if q == 0 then "" else show (q - 1)
  in greekLetters !! r ++ suffix
