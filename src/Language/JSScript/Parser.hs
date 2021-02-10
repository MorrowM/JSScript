module Language.JSScript.Parser where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor.Identity
import Data.Text (Text, pack)
import Language.JSScript.AST
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

lexer :: P.TokenParser a
lexer =
  P.makeTokenParser
    javaStyle
      { P.reservedNames =
          [ "while",
            "if",
            "else",
            "true",
            "false",
            "fn",
            "var",
            "return",
            "break"
          ],
        P.reservedOpNames =
          [ "+",
            "*",
            "==",
            "!=",
            "="
          ]
      }

ident = pack <$> P.identifier lexer

litText = pack <$> P.stringLiteral lexer

litInt = P.integer lexer

litDouble = P.float lexer

symbol = P.symbol lexer

lexeme = P.lexeme lexer

whiteSpace = P.whiteSpace lexer

parens = P.parens lexer

braces = P.braces lexer

semi = P.semi lexer

commaSep = P.commaSep lexer

argList :: Parser ArgList
argList = parens (commaSep ident)

exprList :: Parser ExprList
exprList = parens (commaSep expr)

lit :: Parser Lit
lit = (LitText <$> litText) <|> try litBool <|> try (LitDouble <$> litDouble) <|> (LitInt . fromInteger <$> litInt)
  where
    litBool = LitBool <$> ((True <$ symbol "true") <|> (False <$ symbol "false"))

expr :: Parser Expr
expr = exprEqual
  where
    exprLit = ExprLit <$> lit
    exprVar = ExprVar <$> ident
    exprFuncCall = ExprFuncCall <$> ident <*> exprList
    exprParens = parens expr
    exprSimple = exprLit <|> try exprFuncCall <|> exprVar <|> exprParens
    exprProd = foldr1 ExprProd <$> sepBy1 exprSimple (symbol "*")
    exprSum = foldr1 ExprSum <$> sepBy1 exprProd (symbol "+")
    exprNEqual = foldr1 ExprNEqual <$> sepBy1 exprSum (symbol "!=")
    exprEqual = foldr1 ExprEqual <$> sepBy1 exprNEqual (symbol "==")

stmt :: Parser Stmt
stmt = block <|> declare <|> try ifstmt <|> while <|> breakstmt <|> func <|> returnstmt <|> try assign <|> funcCall
  where
    block = StmtBlock <$> braces (many stmt)
    declare = StmtDeclare <$> (symbol "var" *> ident) <*> (symbol "=" *> expr <* semi)
    assign = StmtAssign <$> (ident <* symbol "=") <*> (expr <* semi)
    ifstmt = StmtIf <$> (symbol "if" *> parens expr) <*> stmt <*> optionMaybe (symbol "else" *> stmt)
    while = StmtWhile <$> (symbol "while" *> parens expr) <*> stmt
    func = StmtFunc <$> (symbol "fn" *> ident) <*> argList <*> braces (many stmt)
    returnstmt = StmtReturn <$> (symbol "return" *> expr <* semi)
    funcCall = StmtFuncCall <$> ident <*> exprList <* semi
    breakstmt = StmtBreak <$ (symbol "break" *> semi)