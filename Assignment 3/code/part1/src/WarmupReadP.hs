module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--  E    ::= Eopt T'
--  E' ::= "+" E' T' | "-" E' T' | epsilon
--  T'   ::= T | "-" T
--  T    ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString = undefined


lexeme :: Parser a -> Parser a
lexeme = undefined

symbol :: String -> Parser ()
symbol = undefined 

num :: Parser Int
num = undefined

addOp :: Parser (Exp -> Exp -> Exp)
addOp :: undefined

negOp :: Parser (Exp -> Exp -> Exp)
negOp :: undefined


expr :: Parser Exp
expr = undefined


expr' :: Exp -> Parser Exp
expr' = undefined

term' :: Parser Exp

term :: Exp -> Parser Exp
term = undefined 