module Main where

import qualified CPS.Translation as CpsTranslation
import qualified CPS.Inferer as CpsInferer
import qualified Lambda.Parser as LambdaParser
import qualified Lambda.Inferer as LambdaInferer
import qualified CPS.Translation as CPSTranslation

main :: IO ()
main = do
  putStrLn "Enter a lambda expression:"
  input <- getLine
  main2 input

main2 :: String -> IO ()
main2 input = do
  mainHelper input

mainHelper :: String -> IO ()
mainHelper input = 
  case LambdaParser.parseExpression input of
    Left err -> print err
    Right e -> do
      let (lambdaInferedType, _) = LambdaInferer.inferExpr e
      let cbvExpected = CPSTranslation.cbvTypeTranslation lambdaInferedType
      let cbnExpected = CPSTranslation.cbnTypeTranslation lambdaInferedType

      let cbvCpsExpr = CpsTranslation.cbvExprTranslation e
      let cbvActual = CpsInferer.runTI (CpsInferer.inferWithCtx cbvCpsExpr)

      let cbnCpsExpr = CpsTranslation.cbnExprTranslation e
      let cbnActual = CpsInferer.runTI (CpsInferer.inferWithCtx cbnCpsExpr)

      putStrLn "Lambda Expression:"
      print e
      putStrLn "Expression Type:"
      print lambdaInferedType

      putStrLn "\nCall-by-value CPS Translation:"
      print cbvCpsExpr
      putStrLn "Expected Type:"
      print cbvExpected
      putStrLn "Inferred Type:"
      either print print cbvActual

      putStrLn "\nCall-by-name CPS Translation:"
      print cbnCpsExpr
      putStrLn "Expected Type:"
      print cbnExpected
      putStrLn "Inferred Type:"
      either print print cbnActual

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
