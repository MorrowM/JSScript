module Language.JSScript.AST where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Vector as V

type VarTable = Map.Map Ident Any

type EvalM = ExceptT Text (StateT VarTable IO)

newtype Foreign = Foreign ([Any] -> EvalM ())

instance Show Foreign where
  show = const "<foreign function>"

instance Eq Foreign where
  (==) _ _ = False

type Ident = Text

type Block = [Stmt]

type ArgList = [Ident]

type ExprList = [Expr]

data Lit
  = LitInt Int
  | LitDouble Double
  | LitText Text
  | LitBool Bool
  | LitVec (V.Vector Expr)
  deriving (Eq, Show)

data Stmt
  = StmtBlock Block
  | StmtDeclare Ident Expr
  | StmtAssign Ident Expr
  | StmtIf Expr Stmt (Maybe Stmt)
  | StmtWhile Expr Stmt
  | StmtFunc Ident ArgList Block
  | StmtReturn Expr
  | StmtFuncCall Ident ExprList
  | StmtBreak
  | StmtForeign ExprList Foreign
  | StmtImport FilePath
  deriving (Eq, Show)

data Expr
  = ExprLit Lit
  | ExprVar Ident
  | ExprFuncCall Ident ExprList
  | ExprIndex Expr Expr
  | ExprSum Expr Expr
  | ExprDiv Expr Expr
  | ExprProd Expr Expr
  | ExprEqual Expr Expr
  | ExprNEqual Expr Expr
  deriving (Eq, Show)

data Any
  = AInt Int
  | ABool Bool
  | ADouble Double
  | AText Text
  | AVec (V.Vector Any)
  | AFunc ArgList Block Expr
  deriving (Eq, Show)

anyToExpr :: Any -> Expr
anyToExpr = \case
  AInt x -> ExprLit $ LitInt x
  ABool x -> ExprLit $ LitBool x
  ADouble x -> ExprLit $ LitDouble x
  AText x -> ExprLit $ LitText x
  AFunc {} -> ExprLit $ LitText "<function>"
  AVec x -> ExprLit $ LitVec (anyToExpr <$> x)

anyToName :: Any -> Text
anyToName = \case
  AInt _ -> "int"
  ABool _ -> "bool"
  ADouble _ -> "double"
  AText _ -> "string"
  AFunc {} -> "function"
  AVec _ -> "array"

anyToString :: Any -> String
anyToString (AInt x) = show x
anyToString (ABool x) = if x then "true" else "false"
anyToString (ADouble x) = show x
anyToString (AText x) = show x
anyToString AFunc {} = "<function>"
anyToString (AVec x) = "[" <> intercalate "," (anyToString <$> V.toList x) <> "]"