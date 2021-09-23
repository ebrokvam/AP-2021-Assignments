module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion: copy-paste from readP
--  E    ::= Eopt T'
--  E' ::= "+" E' T' | "-" E' T' | epsilon
--  T'   ::= T | "-" T
--  T    ::= num | "(" E ")"

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError

type Parser a = Parsec a
type PsrseError = String

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

-- Optional: if not attempted, leave as undefined
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


term :: Parser Exp
term = undefined 