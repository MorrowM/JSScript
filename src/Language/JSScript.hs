module Language.JSScript (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Language.JSScript.AST
import Language.JSScript.Interpreter
import Language.JSScript.Parser
import System.Environment
import Text.Parsec
import System.Console.Haskeline

main :: IO ()
main = do
  mf <- getArgs
  case mf of
    [] -> void $ execStateT interactive stdlib
    (f:_) -> do
      toParse <- readFile f
      let mstmt = parse stmt f ("{ " <> toParse <> " }")
      case mstmt of
        Left err -> print err
        Right stmt -> void $ flip execStateT stdlib $ do
          res <- runExceptT $ exec stmt
          case res of
            Left err -> lift $ putStrLn $ "error: " <> unpack err
            Right _ -> pure ()
  

stdlib :: VarTable
stdlib =
  Map.fromList
    [ ("print", printFunc)
    ]

printFunc :: Any
printFunc =
  AFunc
    ["x"]
    [ StmtForeign [ExprVar "x"] (Foreign $ liftIO . traverse_ (putStrLn . anyToString))
    ]
    (ExprLit $ LitBool True)

anyToString :: Any -> String
anyToString (AInt x) = show x
anyToString (ABool x) = show x
anyToString (ADouble x) = show x
anyToString (AText x) = unpack x
anyToString AFunc {} = "<function>"

handleErrorsInteractive_ :: MonadIO m => ExceptT Text m () -> InputT m ()
handleErrorsInteractive_ e = do
  res <- lift $ runExceptT e
  case res of
    Left err -> outputStrLn $ unpack err
    Right res -> pure res

handleErrorsInteractive :: MonadIO m => ExceptT Text m Any -> InputT m ()
handleErrorsInteractive e = do
  res <- lift $ runExceptT e
  case res of
    Left err -> outputStrLn $ "error: " <> unpack err
    Right res -> outputStrLn $ anyToString res


interactive :: StateT VarTable IO ()
interactive = runInputT defaultSettings loop
  where 
    loop :: InputT (StateT VarTable IO) ()
    loop = do
      line <- getInputLine "> "
      case line of
        Nothing -> pure ()
        Just inp -> case parse stmt "" inp of
          Right s -> handleErrorsInteractive_ $ exec s
          Left _ -> case parse expr "" inp of
            Left _ -> outputStrLn "error: invalid syntax"
            Right ex -> handleErrorsInteractive (eval ex)
      loop 