module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--  E    ::= Eopt T'
--  E'   ::= "+" E' T' | "-" E' T' | epsilon
--  T'   ::= T | "-" T
--  T    ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = 
  case readP_to_S fullExpr s of 
    [(a,_)] -> Right a
    _ -> Left "parse error"


fullExpr :: Parser Exp
fullExpr = do e <- expr; eof; return e

num :: Parser Int
num = lexeme $ do n <- many1 (satisfy isDigit); skipSpaces; return $ read n

symbol :: String -> Parser ()
symbol ps = lexeme $ do _ <- string ps; skipSpaces   -- fra 2021

addOp :: Parser (Exp -> Exp -> Exp)
addOp = do symbol "+"; return Add

negOp :: Parser (Exp -> Exp)
negOp = do symbol "-"; return Negate

expr :: Parser Exp
expr = do e <- term'; expr' e 

expr' :: Exp -> Parser Exp
expr' e' = do add <- addOp; e'' <- term'; expr' (add e' e'') 
          <|> do neg <- negOp; e'' <- term'; expr' (Add e' (neg e''))
          <|> return e'

term' :: Parser Exp
term' = term
        <|> do symbol "-"; Negate <$> term

term :: Parser Exp
term = Num <$> num
       <|> do symbol "("; t <- expr; symbol ")"; return t

whitespace :: Parser ()
whitespace = do _ <-  many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a