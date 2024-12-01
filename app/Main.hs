module Main where

import qualified Parser

main :: IO ()
main = putStrLn "Hello, Haskell!"

parseExpr :: String -> IO ()
parseExpr s = case Parser.parseExpression s of
  Left er -> print er
  Right e -> print e
