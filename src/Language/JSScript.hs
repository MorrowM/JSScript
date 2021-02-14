{-# LANGUAGE TemplateHaskell #-}

module Language.JSScript where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.FileEmbed
import Data.Foldable
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Language.JSScript.AST
import Language.JSScript.Interpreter
import Language.JSScript.Parser
import System.Console.Haskeline
import System.Environment
import Text.Parsec
import Data.List

main :: IO ()
main = do
  mf <- getArgs
  stdlib <- stdlibIO
  case mf of
    [] -> void $ execStateT interactive stdlib
    (f : _) -> do
      toParse <- readFile f
      let mstmts = parse (many stmt <* eof) f toParse
      case mstmts of
        Left err -> print err
        Right stmts -> void $
          flip execStateT stdlib $ do
            res <- runExceptT $ traverse_ exec stmts
            case res of
              Left err -> lift $ putStrLn $ "error: " <> unpack err
              Right _ -> pure ()

stdlibStr :: String
stdlibStr = $(embedStringFile "stdlib/prelude.jss")

stdlibIO :: IO VarTable
stdlibIO = do
  let baselib =
        Map.fromList
          [ ("print", printFunc),
            ("globals", globalsFunc),
            ("length", lengthFunc)
          ]
  Right stmts <- pure $ parse (many stmt <* eof) "" stdlibStr
  vtable <- flip execStateT baselib $ do
    Right _ <- runExceptT $ traverse_ exec stmts
    pure ()
  pure $ vtable `Map.union` baselib

printFunc :: Any
printFunc =
  AFunc
    ["x"]
    [ StmtForeign [ExprVar "x"] (Foreign $ liftIO . traverse_ (putStrLn . anyToString))
    ]
    (ExprLit $ LitBool True)

globalsFunc :: Any
globalsFunc =
  AFunc
    []
    [ StmtForeign
        []
        ( Foreign $
            const $ do
              g <- fmap anyToString <$> lift get
              exec $ StmtDeclare "globals_var" (ExprLit $ LitText $ showt g)
        )
    ]
    (ExprVar "globals_var")

lengthFunc :: Any 
lengthFunc = AFunc ["arr"] 
  [ StmtForeign [ExprVar "arr"] $ Foreign $ \arr -> do
    val <- annotate "incorrect number of arguments passed to length" $ case arr of { [x] -> Just x ; _ -> Nothing }
    res <- case val of
      AVec v -> pure $ V.length v
      _ -> throwE "cannot get length of a non-array"
    exec $ StmtDeclare "arr_length" (ExprLit $ LitInt res)
  ] (ExprVar "arr_length")

anyToString :: Any -> String
anyToString (AInt x) = show x
anyToString (ABool x) = if x then "true" else "false"
anyToString (ADouble x) = show x
anyToString (AText x) = show x
anyToString AFunc {} = "<function>"
anyToString (AVec x) = "[" <> intercalate "," (anyToString <$> V.toList x) <> "]"

handleErrorsInteractive_ :: MonadIO m => ExceptT Text m () -> InputT m ()
handleErrorsInteractive_ e = do
  res <- lift $ runExceptT e
  case res of
    Left err -> outputStrLn $ unpack err
    Right res' -> pure res'

handleErrorsInteractive :: MonadIO m => ExceptT Text m Any -> InputT m ()
handleErrorsInteractive e = do
  res <- lift $ runExceptT e
  case res of
    Left err -> outputStrLn $ "error: " <> unpack err
    Right res' -> outputStrLn $ anyToString res'

interactive :: StateT VarTable IO ()
interactive = runInputT defaultSettings loop
  where
    loop :: InputT (StateT VarTable IO) ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing -> pure ()
        Just inp -> case parse (many stmt <* eof) "" inp of
          Right s -> handleErrorsInteractive_ $ exec (StmtBlock s)
          Left _ -> case parse (expr <* eof) "" inp of
            Left _ -> outputStrLn "error: invalid syntax"
            Right ex -> handleErrorsInteractive (eval ex)
      loop
