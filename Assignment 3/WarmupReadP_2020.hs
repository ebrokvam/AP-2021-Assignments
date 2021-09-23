module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E   ::= T E' .
-- E'  ::= "+" T E' | "-" T E' | epsilon
-- T   ::= num | "-" E | "(" E ")"

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
  case readP_to_S (do whitespace; e <- expr; eof; return e) s of 
    [(a,_)] -> Right a
    [] -> Left "input expression is ill formed"
    _ -> Left "ops I did it again, my grammar is bad"

whitespace :: Parser ()
whitespace = do _ <- many (satisfy isSpace); return () --need to include \t and \n

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do _ <- string s; return ()

num :: Parser Int
num = lexeme $ do ds <- many1 (satisfy isDigit); return $ read ds 

addOp :: Parser (Exp -> Exp -> Exp)
addOp = do symbol "+"; return Add

negOp :: Parser (Exp -> Exp)        
negOp = do symbol "-"; return Negate

expr :: Parser Exp
expr = do e <- term; expr' e

expr' :: Exp -> Parser Exp
expr' e1 = do ao <- addOp; e2 <- term; expr' (ao e1 e2)
          <|> do no <- negOp; e2 <- term; expr' (Add e1 (no e2))
          <|> return e1

term :: Parser Exp
term = do n <- num; return $ Num n
        <|> do symbol "-"; e <- expr; return $ Negate e
        <|> do symbol "("; e <- expr; symbol ")"; return e


-- aux function
--if two succ symbol is not digit then error

-- Parsing: "\t \n - 1 + 2 - ( 3 ) "
--   expected: Right (Add (Add (Negate (Num 1)) (Num 2)) (Negate (Num 3)))
--   but got: Left "ops I did it again, my grammar is bad"

-- Parsing: "--3"
--   expected: Left "<message>"
--   but got: Right (Negate (Negate (Num 3)))

-- Parsing: "2+-3"
--   expected: Left "<message>"
--   but got: Right (Add (Num 2) (Negate (Num 3)))