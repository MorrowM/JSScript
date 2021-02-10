module Language.JSScript (main) where


import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import Language.JSScript.AST
import qualified Data.Map as Map
import Control.Monad.Trans.Class
import Control.Monad
import Data.Foldable
import Data.Functor
import Language.JSScript.Interpreter
import Language.JSScript.Parser
import Control.Monad.IO.Class
import System.Environment
import Text.Parsec

main :: IO ()
main = do
  [f] <- getArgs
  toParse <- readFile f 
  Right stmt <- pure $ parse stmt f ("{ " <> toParse <> " }")
  void $flip execStateT stdlib $ do
    res <- runExceptT $ exec stmt
    case res of
      Left err -> lift $ putStrLn $ "error: " <> unpack err
      Right _ -> pure ()

   
stdlib :: VarTable
stdlib = Map.fromList
  [ ("print", printFunc) 
  ]

printFunc :: Any
printFunc = AFunc ["x"] 
  [ StmtForeign [ExprVar "x"] (Foreign $ liftIO . traverse_ (putStrLn . anyToString))
  ] (ExprLit $ LitBool True)

anyToString :: Any -> String
anyToString (AInt x) = show x
anyToString (ABool x) = show x
anyToString (ADouble x) = show x
anyToString (AText x) = unpack x
anyToString AFunc {} = "<function>"