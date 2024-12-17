module Main where

import qualified CPS
import qualified Parser

main :: IO ()
main = putStrLn "Hello, Haskell!"

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
parseExpr s = case Parser.parseExpression s of
  Left er -> print er
  Right e -> print e

callByValueTranslate :: String -> IO ()
callByValueTranslate s = case Parser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda ADT Representation:"
    print e
    putStrLn "Lambda Expression:"
    print s
    let cpsExpr = CPS.callByValueToCps e
    putStrLn "CPS ADT Representation:"
    print cpsExpr
    putStrLn "CPS Expression:"
    print $ CPS.prettyPrintCPS cpsExpr

callByNameTranslate :: String -> IO ()
callByNameTranslate s = case Parser.parseExpression s of
  Left er -> putStrLn $ "Error: " ++ show er
  Right e -> do
    putStrLn "Lambda ADT Representation:"
    print e
    putStrLn "Lambda Expression:"
    print s
    let cpsExpr = CPS.callByNameToCps e
    putStrLn "CPS ADT Representation:"
    print cpsExpr
    putStrLn "CPS Expression:"
    print $ CPS.prettyPrintCPS cpsExpr
