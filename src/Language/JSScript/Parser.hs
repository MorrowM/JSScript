module Language.JSScript.Parser where

import Data.Text (Text, pack)
import qualified Data.Vector as V
import Language.JSScript.AST
import Text.Parsec
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
            "=",
            "/"
          ],
        P.identLetter = alphaNum <|> oneOf "_'-"
      }

ident :: Parser Text
ident = pack <$> P.identifier lexer

litText :: Parser Text
litText = pack <$> P.stringLiteral lexer

litInt :: Parser Integer
litInt = P.integer lexer

litDouble :: Parser Double
litDouble = P.float lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

semi :: Parser String
semi = P.semi lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

argList :: Parser ArgList
argList = parens (commaSep ident)

exprList :: Parser ExprList
exprList = parens (commaSep expr)

lit :: Parser Lit
lit = (LitText <$> litText) <|> try litBool <|> try (LitDouble <$> litDouble) <|> (LitInt . fromInteger <$> litInt) <|> litVec
  where
    litBool = LitBool <$> ((True <$ symbol "true") <|> (False <$ symbol "false"))
    litVec = LitVec . V.fromList . fmap ExprLit <$> between (symbol "[") (symbol "]") (commaSep lit)

expr :: Parser Expr
expr = exprEqual
  where
    exprLit = ExprLit <$> lit
    exprVar = ExprVar <$> ident
    exprFuncCall = ExprFuncCall <$> ident <*> exprList
    exprParens = parens expr
    exprIndex = ExprIndex <$> (exprLit <|> exprParens <|> try exprFuncCall <|> exprVar) <*> between (symbol "[") (symbol "]") expr
    exprSimple = try exprIndex <|> try exprLit <|> exprParens <|> try exprFuncCall <|> try exprVar
    exprProd = foldr1 ExprProd <$> sepBy1 exprSimple (symbol "*")
    exprDiv = foldr1 ExprDiv <$> sepBy1 exprProd (symbol "/")
    exprSum = foldr1 ExprSum <$> sepBy1 exprDiv (symbol "+")
    exprNEqual = foldr1 ExprNEqual <$> sepBy1 exprSum (symbol "!=")
    exprEqual = foldr1 ExprEqual <$> sepBy1 exprNEqual (symbol "==")

stmt :: Parser Stmt
stmt = block <|> try declare <|> try ifstmt <|> try while <|> try breakstmt <|> try func <|> try returnstmt <|> try importstmt <|> try assign <|> funcCall
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
    importstmt = StmtImport <$> (symbol "import" *> many1 alphaNum <* semi)
