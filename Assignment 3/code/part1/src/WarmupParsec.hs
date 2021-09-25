module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion: copy-paste from readP
--  E    ::= T' E' 
--  E'   ::= "+" T' E'  | "-" T' E'  | epsilon
--  T'   ::= T | "-" T
--  T    ::= num | "(" E ")"

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Text.ParserCombinators.Parsec.Prim()

type PsrseError = String

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

-- Optional: if not attempted, leave as undefined
parseString :: String -> Either ParseError Exp
parseString = parse fullExpr "parse error"


fullExpr :: Parser Exp
fullExpr = do e <- expr; eof; return e

num :: Parser Int
-- num = do n <- many1 digit; spaces; return $ read n
num = lexeme $ do n <- many1 digit; spaces; return $ read n

symbol :: String -> Parser ()
-- symbol ps = do _ <- try $ string ps; spaces
symbol ps = lexeme $ do _ <- try $ string ps; spaces

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
whitespace = do _ <- space; _ <- newline; _ <- tab; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

