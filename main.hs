import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

data Expr
  = Var String
  | Num Integer
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Assign String Expr
  | If Expr [Expr] [Expr] -- if condition then-branch else-branch
  | While Expr [Expr]     -- while condition loop-body
  | FuncDecl String [String] [Expr] -- function declaration: name, parameters, body
  | FuncCall String [Expr] -- function call: name, arguments
  deriving (Show)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    ops = ["+", "-", "*", "/"]
    names = ["int", "var", "if", "else", "while", "func"]
    style = emptyDef
      { Token.commentLine = "//"
      , Token.reservedOpNames = ops
      , Token.reservedNames = names
      }

integer :: Parser Integer
integer = Token.integer lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

expr :: Parser Expr
expr = try assignExpr <|> addSubExpr <|> mulDivExpr <|> ifExpr <|> whileExpr <|> funcDeclExpr <|> funcCallExpr

assignExpr :: Parser Expr
assignExpr = do
  var <- identifier
  reserved "="
  val <- addSubExpr
  return $ Assign var val

addSubExpr :: Parser Expr
addSubExpr = chainl1 term $ addOp "+" <|> addOp "-"
  where
    term = parens expr <|> Var <$> identifier <|> Num <$> integer

mulDivExpr :: Parser Expr
mulDivExpr = chainl1 factor $ addOp "*" <|> addOp "/"
  where
    factor = parens expr <|> addSubExpr

addOp :: String -> Parser (Expr -> Expr -> Expr)
addOp op = reserved op >> return (if op == "+" then Add else if op == "-" then Sub else Mul)

ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- parens expr
  thenBranch <- braces $ semiSep expr
  elseBranch <- option [] (reserved "else" >> braces (semiSep expr))
  return $ If cond thenBranch elseBranch

whileExpr :: Parser Expr
whileExpr = do
  reserved "while"
  cond <- parens expr
  body <- braces $ semiSep expr
  return $ While cond body

funcDeclExpr :: Parser Expr
funcDeclExpr = do
  reserved "func"
  name <- identifier
  params <- parens $ sepBy identifier (reserved ",")
  body <- braces $ semiSep expr
  return $ FuncDecl name params body

funcCallExpr :: Parser Expr
funcCallExpr = do
  name <- identifier
  args <- parens $ sepBy expr (reserved ",")
  return $ FuncCall name args

program :: Parser [Expr]
program = semiSep expr

parseCSubset :: String -> Either ParseError [Expr]
parseCSubset input = parse program "" input
