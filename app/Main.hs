module Main where

import qualified CPS.Inferer as CPSInferer
import qualified Lambda.Parser as LambdaParser
import qualified Lambda.Inferer as LambdaInferer
import qualified CPS.Translation as CPSTranslation
import qualified Lambda.Typing as LambdaTyping
import qualified Utils.Typing as Utils
import qualified CPS.Typing as CPSTyping
import          System.Directory (createDirectoryIfMissing)
import          System.FilePath (takeBaseName, (<.>), (</>))

main :: IO ()
main = putStrLn "Input file path:" >> getLine >>= inferFromFile

inferFromFile :: String -> IO ()
inferFromFile filename = do
  file <- readFile filename
  inferExpr filename file

inferExpr :: FilePath -> String -> IO ()
inferExpr srcPath input = do
  mainHelper srcPath input

mainHelper :: FilePath -> String -> IO ()
mainHelper srcPath input =
  case LambdaParser.parseExpression input of
    Left err -> print err
    Right lambdaExpr -> do
      putStrLn "Expression:"
      print lambdaExpr

      putStrLn "Type:"
      let (lambdaType, _) = LambdaInferer.inferExpr lambdaExpr
      print lambdaType

      putStrLn "Call-by-Name Translation:"
      cbnCommand <- inferAndCheck lambdaExpr Utils.CBN

      putStrLn "\nCall-by-Value Translation:"
      cbvCommand <- inferAndCheck lambdaExpr Utils.CBV

      outFile <- saveInOutput srcPath (generateOutputBody cbnCommand cbvCommand)

      putStrLn $ "\n" ++ "CPS expression saved in " ++ outFile

      return ()

inferAndCheck :: LambdaTyping.Expr -> Utils.CallStyle -> IO CPSTyping.Command
inferAndCheck expr callStyle = do
  let (lambdaType, _) = LambdaInferer.inferExpr expr
      (expectedCPSType, command) = getTypeAndCommand callStyle lambdaType expr

  putStrLn "Command:"
  print command
  putStrLn "Expected Continuation Type:"
  print expectedCPSType

  handleInferenceResult command expectedCPSType
  return command

getTypeAndCommand :: Utils.CallStyle -> LambdaTyping.LambdaMonoType -> LambdaTyping.Expr 
                  -> (CPSTyping.CPSPolyType, CPSTyping.Command)
getTypeAndCommand callStyle lambdaType expr =
  case callStyle of
    Utils.CBN -> (CPSTranslation.cbnTypeTranslation lambdaType,
                 CPSTranslation.cbnExprTranslation expr)
    Utils.CBV -> (CPSTranslation.cbvTypeTranslation lambdaType,
                 CPSTranslation.cbvExprTranslation expr)

handleInferenceResult :: CPSTyping.Command -> CPSTyping.CPSPolyType -> IO ()
handleInferenceResult command expectedType = do
  let inferredType = CPSInferer.runTI (CPSInferer.inferWithCtx command)

  case inferredType of
    Left err -> handleInferenceError err
    Right inferredType' -> handleSuccessfulInference inferredType' expectedType

handleInferenceError :: CPSInferer.TypeError -> IO ()
handleInferenceError err = do
  putStrLn "Type Error in Inference:"
  print err

handleSuccessfulInference :: CPSTyping.CPSPolyType -> CPSTyping.CPSPolyType -> IO ()
handleSuccessfulInference inferredType expectedType = do
  putStrLn "Inferred Continuation Type:"
  print inferredType
  case CPSInferer.isSubtypeOfPoly inferredType expectedType of
    Left err -> handleSubtypeError err
    Right maybeSubtype -> handleSubtypeResult maybeSubtype

handleSubtypeError :: CPSInferer.TypeError -> IO ()
handleSubtypeError err = do
  putStrLn "Type Error in Subtyping Check:"
  print err

handleSubtypeResult :: Maybe a -> IO ()
handleSubtypeResult maybeSubtype = do
  putStrLn "Do the types match?"
  case maybeSubtype of
    Nothing -> putStrLn "No"
    Just _ -> putStrLn "Yes"

saveInOutput
  :: FilePath
  -> String
  -> IO FilePath
saveInOutput srcPath body = do
  let base       = takeBaseName srcPath
      dir        = "output"
      outFile    = dir </> base <.> "hs"
      header     = unlines
        [ "-- Auto-generated by Main.hs"
        , "-- For input " ++ base
        , "-- This code works only for Church Encoding expressions"
        , "-- It will compute the ..."
        , "-- ... generated CPS translated expression Church enconding"
        , ""
        , "-----------------------------------------------------------"
        ]
      fullSource = header ++ "\n" ++ body

  createDirectoryIfMissing True dir
  writeFile outFile fullSource
  return outFile

generateOutputBody :: CPSTyping.Command -> CPSTyping.Command -> String
generateOutputBody cbnCommand cbvCommand = unlines
  [ "cbn k = " ++ CPSTyping.flatPrintThielecke cbnCommand
  , ""
  , "cbv k = " ++ CPSTyping.flatPrintThielecke cbvCommand
  , ""
  , "-----------------------------------------------------------"
  , ""
  , "inc_cbv :: (Int, Int -> r) -> r"
  , "inc_cbv (n, k) = k (1 + n)"
  , ""
  , "test_cbv = "
  , "    cbv (\\f -> f (inc_cbv, \\x -> x (0, id)))"
  , ""
  , "thunk :: a -> (a -> r) -> r"
  , "thunk x k = k x"
  , ""
  , "inc_cbn :: (Int -> r, Int) -> r"
  , "inc_cbn (k, n) ="
  , "    inc_cbv (n, k)"
  , ""
  , "test_cbn ="
  , "    cbn (\\f -> f (thunk inc_cbn, \\x -> x (id, 0)))"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  print $ (test_cbn, test_cbv)"
  ]

identity :: String
identity = "\\x. x"

churchZero :: String
churchZero = "\\f. \\x. x"

churchOne :: String
churchOne = "\\f. \\x. f x"

churchTwo :: String
churchTwo = "\\f. \\x. f (f x)"

sCombinator :: String
sCombinator = "\\x. \\y. \\z. x z (y z)"
