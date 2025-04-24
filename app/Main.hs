module Main where

import qualified CPS.Inferer as CpsInferer
import qualified Lambda.Parser as LambdaParser
import qualified Lambda.Inferer as LambdaInferer
import qualified CPS.Translation as CPSTranslation
import qualified Lambda.Typing as LambdaTyping
import qualified Utils.Typing as Utils
import qualified CPS.Typing as CpsInferer

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
    Right lambdaExpr -> do
      putStrLn "Expression:"
      print lambdaExpr

      putStrLn "Call-by-Name Translation:"
      inferAndCheck lambdaExpr Utils.CBN
      putStrLn "\nCall-by-Value Translation:"
      inferAndCheck lambdaExpr Utils.CBV

inferAndCheck :: LambdaTyping.Expr -> Utils.CallStyle -> IO ()
inferAndCheck expr callStyle = do
  let (lambdaType, _) = LambdaInferer.inferExpr expr
      (expectedCPSType, command) = getTypeAndCommand callStyle lambdaType expr

  putStrLn "Command:"
  print command
  putStrLn "Expected Type:"
  print expectedCPSType

  handleInferenceResult command expectedCPSType

getTypeAndCommand :: Utils.CallStyle -> LambdaTyping.SimpleType -> LambdaTyping.Expr 
                  -> (CpsInferer.PolyType, CpsInferer.Command)
getTypeAndCommand callStyle lambdaType expr =
  case callStyle of
    Utils.CBN -> (CPSTranslation.cbnTypeTranslation lambdaType, 
                 CPSTranslation.cbnExprTranslation expr)
    Utils.CBV -> (CPSTranslation.cbvTypeTranslation lambdaType,
                 CPSTranslation.cbvExprTranslation expr)

handleInferenceResult :: CpsInferer.Command -> CpsInferer.PolyType -> IO ()
handleInferenceResult command expectedType = do
  let inferredType = CpsInferer.runTI (CpsInferer.inferWithCtx command)
  
  case inferredType of
    Left err -> handleInferenceError err
    Right inferredType' -> handleSuccessfulInference inferredType' expectedType

handleInferenceError :: CpsInferer.TypeError -> IO ()
handleInferenceError err = do
  putStrLn "Type Error in Inference:"
  print err

handleSuccessfulInference :: CpsInferer.PolyType -> CpsInferer.PolyType -> IO ()
handleSuccessfulInference inferredType expectedType = do
  putStrLn "Inferred Type:"
  print inferredType
  case CpsInferer.isSubtypeOfPoly inferredType expectedType of
    Left err -> handleSubtypeError err
    Right maybeSubtype -> handleSubtypeResult maybeSubtype

handleSubtypeError :: CpsInferer.TypeError -> IO ()
handleSubtypeError err = do
  putStrLn "Type Error in Subtyping Check:"
  print err

handleSubtypeResult :: Maybe a -> IO ()
handleSubtypeResult maybeSubtype = do
  putStrLn "Do the types match?"
  case maybeSubtype of
    Nothing -> putStrLn "No"
    Just _ -> putStrLn "Yes"

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
