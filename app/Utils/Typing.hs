module Utils.Typing where
import Control.Monad.State (State, MonadState (get, put))

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

type Id = String

data CallStyle = 
  CBV
  | CBN

greekLetters :: [String]
greekLetters = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
                "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"]

greekVar :: Int -> String
greekVar n =
  let (q, r) = n `divMod` length greekLetters
      suffix = if q == 0 then "" else show (q - 1)
  in greekLetters !! r ++ suffix
