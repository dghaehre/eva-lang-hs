module Frontend.Parser
  ( parseExpr,
    parseToplevel,
  )
where

import Frontend.Lexer
import Frontend.Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

binary s f assoc =
  Ex.Infix (reservedOp s >> return (BinOp f)) assoc

-- TODO: make sure the assoc is correct
table =
  [ [ binary "+" Plus Ex.AssocLeft,
      binary "*" Multiply Ex.AssocLeft,
      binary "/" Divide Ex.AssocLeft,
      binary "-" Minus Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

int :: Parser Expr
int = Float . fromInteger <$> integer

floating :: Parser Expr
floating = Float <$> float

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ many expr
  return $ Call name args

pipeCall :: Parser String
pipeCall = do
  reserved "|>"
  identifier

pipeCalls :: Parser Expr
pipeCalls = do
  start <- orgValue
  pipes <- many1 pipeCall
  return $ PipeCalls start pipes
  where
    orgValue :: Parser Expr
    orgValue =
      try int
        <|> try floating
        <|> try str
        <|> try call
        <|> variable
        <|> parens expr

str :: Parser Expr
str = StringLiteral <$> stringLiteral

variable :: Parser Expr
variable = Var <$> identifier

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
  reserved "="
  body <- expr
  return $ Const name body

-- Currently only allows for one expression..
-- Maybe this should be a functional programming language?
function :: Parser Expr
function = do
  reserved "fn"
  name <- identifier
  args <- many variable
  (consts, body) <- braces functionBody
  return $ Function name args consts body

functionBody :: Parser ([Expr], Expr)
functionBody = do
  cs <- many letVariable
  returnExpression <- expr
  return (cs, returnExpression)

factor :: Parser Expr
factor =
  try pipeCalls
    <|> try floating
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
defn =
  try function
    <|> expr

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
