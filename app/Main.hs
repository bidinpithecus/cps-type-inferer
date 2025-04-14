module Main where

import qualified CPS.Translation as CpsTranslation
import qualified CPS.Inferer as CpsInferer
import qualified CPS.Typing as CpsTyping
import qualified Lambda.Parser as LambdaParser
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "Enter a lambda expression:"
  input <- getLine
  case LambdaParser.parseExpression input of
    Left err -> print err
    Right e -> do
      let cpsExpr = CpsTranslation.callByValueToCps e
      putStrLn "CPS Expression in call by value:"
      print cpsExpr

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

parseExpr :: String -> IO ()
parseExpr s = case LambdaParser.parseExpression s of
  Left er -> print er
  Right e -> print e

callByValueTranslate :: String -> IO ()
callByValueTranslate s = case LambdaParser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda Expression:"
    print e
    let cpsExpr = CpsTranslation.callByValueToCps e
    putStrLn "CPS Expression in call by value:"
    print cpsExpr

callByNameTranslate :: String -> IO ()
callByNameTranslate s = case LambdaParser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda Expression:"
    print e
    let cpsExpr = CpsTranslation.callByNameToCps e
    putStrLn "CPS Expression in call by name:"
    print cpsExpr

callByValueTypeInference :: String -> IO ()
callByValueTypeInference s = case LambdaParser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda Expression:"
    print e
    let cpsExpr = CpsTranslation.callByValueToCps e
    putStrLn "CPS Expression:"
    print cpsExpr
    case CpsInferer.runTI (CpsInferer.inferWithCtx cpsExpr) of
      Left err -> putStrLn $ "Type error: " ++ show err
      Right (subst, finalCtx) -> do
        putStrLn "Inferred substitutions:"
        putStrLn (CpsTyping.prettySubst subst)
        case Map.lookup "k" finalCtx of
          Just (CpsTyping.Forall _ monoType) -> do
            let generalizedK = CpsInferer.generalize (Map.delete "k" finalCtx) monoType
            putStrLn "Generalized type for k:"
            print generalizedK
          Nothing -> putStrLn "k not found in context"

callByNameTypeInference :: String -> IO ()
callByNameTypeInference s = case LambdaParser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda Expression:"
    print e
    let cpsExpr = CpsTranslation.callByNameToCps e
    putStrLn "CPS Expression:"
    print cpsExpr
    case CpsInferer.runTI (CpsInferer.inferWithCtx cpsExpr) of
      Left err -> putStrLn $ "Type error: " ++ show err
      Right (subst, finalCtx) -> do
        putStrLn "Inferred substitutions:"
        putStrLn (CpsTyping.prettySubst subst)
        case Map.lookup "k" finalCtx of
          Just (CpsTyping.Forall _ monoType) -> do
            let generalizedK = CpsInferer.generalize (Map.delete "k" finalCtx) monoType
            putStrLn "Generalized type for k:"
            print generalizedK
          Nothing -> putStrLn "k not found in context"
