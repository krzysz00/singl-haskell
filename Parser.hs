module Parser (singl) where
import Text.ParserCombinators.Parsec 
import Control.Applicative hiding (many, (<|>))
import Data.Char (ord)

import Types

variableChar :: Parser Char
variableChar = noneOf "&@^_$!|\^C()?,~+-*/%"

variableize :: Char -> Integer
variableize = fromIntegral . ord

variable :: Parser Token
variable = (Variable . variableize) <$> variableChar

variableLike :: Char -> (Integer -> Token) -> Parser Token
variableLike c t = (t . variableize) <$ char c <*> variableChar

address :: Parser Token
address = variableLike '&' Address

dereference :: Parser Token
dereference = variableLike '@' Dereference

output :: Parser Token
output = variableLike '^' Output

input :: Parser Token
input = variableLike '_' Input

argument :: Parser Token
argument = Argument <$ char '|'

arithmeticLike :: Char -> (Token -> Token -> Token) -> Parser Token
arithmeticLike c t = do
  char c
  a <- expression
  b <- expression
  return $ t a b

add :: Parser Token
add = arithmeticLike '+' Add

subtract :: Parser Token
subtract = arithmeticLike '-' Subtract

multiply :: Parser Token
multiply = arithmeticLike '*' Multiply

divide :: Parser Token
divide = arithmeticLike '/' Divide

modulo :: Parser Token
modulo = arithmeticLike '%' Modulo

loop :: Parser Token
loop = arithmeticLike '~' Loop

ifLike :: Char -> (Token -> Token -> Token -> Token) -> Parser Token
ifLike c t = do
  char c
  a <- expression
  b <- expression
  c <- expression
  return $ t a b c

eqZero :: Parser Token
eqZero = ifLike '?' EqZero

leqZero :: Parser Token
leqZero = ifLike ',' LeqZero

group :: Parser Token
group = do
  char '('
  r <- many expression
  char ')'
  return $ Group r

assign :: Parser Token
assign = do
  char '$'
  v <- variable <|> dereference
  g <- many expression
  char '!'
  return $ Assign v g

comment :: Parser Token
comment = do 
  char '\^C'
  skipMany (noneOf "\^C") 
  char '\^C'
  e <- expression
  return $ Comment e
expression :: Parser Token
expression = variable <|> address <|> dereference <|> output <|> input
  <|> add <|> Parser.subtract <|> multiply <|> divide <|> modulo 
  <|> argument <|> eqZero <|> leqZero <|> loop
  <|> group <|> assign <|> comment

singl :: Parser [Token]
singl = many expression
