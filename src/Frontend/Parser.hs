module Frontend.Parser
  ( parseExpr
  , parseToplevel
  ) where

import Frontend.Syntax
import Frontend.Lexer

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

-- parse :: String -> Expr
-- parse content = undefined

binary s f assoc =
  Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

int :: Parser Expr
int = do
  i <- integer
  return $ Float (fromInteger i)

floating :: Parser Expr
floating = do
  f <- float
  return $ Float f

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ many expr
  return $ Call name args

str :: Parser Expr
str = do
  s <- stringLiteral
  return $ StringLiteral s

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

setVariable :: Parser Expr
setVariable = do
  reserved "set"
  name <- identifier
  body <- expr
  return $ Const name body

letVariable :: Parser Expr
letVariable = do
  reserved "let"
  name <- identifier
  body <- expr
  return $ Const name body


-- Currently only allows for one expression..
-- Maybe this should be a functional programming language?
function :: Parser Expr
function = do
  reserved "fn"
  name <- identifier
  args <- many variable
  (consts, body) <- braces $ functionBody
  return $ Function name args consts body

functionBody :: Parser ([Expr], Expr)
functionBody = do
  cs <- many $ letVariable
  returnExpression <- expr
  return $ (cs, returnExpression)

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try setVariable
      <|> try str
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r


-- TODO: Need a better name so it is more clear what this does
defn :: Parser Expr
defn = try function
    <|> expr

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
