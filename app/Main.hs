module Main where

import qualified CPS.Translation as CpsTranslation
import qualified CPS.Inferer as CpsInferer
import qualified Lambda.Parser as LambdaParser

main :: IO ()
main = do
  putStrLn "Enter a lambda expression:"
  input <- getLine
  -- let input = lambdaId
  case LambdaParser.parseExpression input of
    Left err -> print err
    Right e -> do
      let cbvCpsExpr = CpsTranslation.callByValueToCps e
      let cbvResult = CpsInferer.runTI (CpsInferer.inferWithCtx cbvCpsExpr)

      let cbnCpsExpr = CpsTranslation.callByNameToCps e
      let cbnResult = CpsInferer.runTI (CpsInferer.inferWithCtx cbnCpsExpr)

      putStrLn "Lambda Expression:"
      print e

      putStrLn "\nCall-by-value CPS Translation:"
      print cbvCpsExpr
      putStrLn "Inferred Type:"
      either print print cbvResult

      putStrLn "\nCall-by-name CPS Translation:"
      print cbnCpsExpr
      putStrLn "Inferred Type:"
      either print print cbnResult

main2 :: String -> IO ()
main2 input = do
  case LambdaParser.parseExpression input of
    Left err -> print err
    Right e -> do
      let cbvCpsExpr = CpsTranslation.callByValueToCps e
      let cbvResult = CpsInferer.runTI (CpsInferer.inferWithCtx cbvCpsExpr)

      let cbnCpsExpr = CpsTranslation.callByNameToCps e
      let cbnResult = CpsInferer.runTI (CpsInferer.inferWithCtx cbnCpsExpr)

      putStrLn "Lambda Expression:"
      print e

      putStrLn "\nCall-by-value CPS Translation:"
      print cbvCpsExpr
      putStrLn "Inferred Type:"
      either print print cbvResult

      putStrLn "\nCall-by-name CPS Translation:"
      print cbnCpsExpr
      putStrLn "Inferred Type:"
      either print print cbnResult

var :: String
var = "x"

lam :: String
lam = "\\x. e"

app :: String
app = "f e"

lambdaId :: String
lambdaId = "\\x. x"

churchZero :: String
churchZero = "\\f. \\x. x"

churchOne :: String
churchOne = "\\f. \\x. f x"

churchTwo :: String
churchTwo = "\\f. \\x. f (f x)"

sCombinator :: String
sCombinator = "\\x. \\y. \\z. x z (y z)"
