module Language.JSScript.AST where

import Data.Text (Text)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Data.Map as Map


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
  deriving (Eq, Show)

data Expr
  = ExprLit Lit
  | ExprVar Ident
  | ExprFuncCall Ident ExprList
  | ExprSum Expr Expr
  | ExprProd Expr Expr
  | ExprEqual Expr Expr
  | ExprNEqual Expr Expr
  deriving (Eq, Show)

data Any
  = AInt Int
  | ABool Bool
  | ADouble Double
  | AText Text
  | AFunc ArgList Block Expr
  deriving (Eq, Show)
