-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Text.ParserCombinators.Parsec.Prim()
import Data.Char
-- add any other other imports you need

{-
these submodules are allowed
  - Prim        - tjek
  - Char
  - Error
  - String
  - Combinator 

these submodules are NOT allowed (sad:face)
  - Token
  - Expr (has buildExpressionParser - so so nice functionality)
  - Language
-}

-- type ParseError = String -- you may replace this


parseString :: String -> Either ParseError Program
parseString = parse stmtS "parse error"

stmtS :: Parser Program
stmtS = try (do s <- stmt; eof; return [s])
       <|> do s' <- stmt; symbol ";"; s'' <- stmtS; eof; return (s':s'')

stmt :: Parser Stmt
stmt = try (do v <- ident; symbol "="; SDef v <$> expr')
      <|> do SExp <$> expr'


exprS :: Parser Exp
-- exprS = do e <- expr; eof; return e
exprS = try (do e' <- expr'; symbol ","; e'' <- exprS; return $ List [e',e''])
        <|> expr'

expr' :: Parser Exp
expr' = do e <- expr; negOp e

expr :: Parser Exp
expr = do e <- term'; relOp e

term' :: Parser Exp
term' = do t' <- term; addOp t'

term :: Parser Exp
term = do t <- factor'; multOp t

factor' :: Parser Exp
factor' = factor
       <|> do symbol "-"; factor

factor :: Parser Exp
factor = Const . IntVal <$> numConst
       <|> Var <$> ident
       <|> do symbol "'"; s <- stringConst; symbol "'"; return $ Const $ StringVal s
       <|> do symbol "'"; symbol "'"; return $ Const $ StringVal "''"
       <|> do symbol "None"; return $ Const NoneVal
       <|> do symbol "True"; return $ Const TrueVal
       <|> do symbol "False"; return $ Const FalseVal
       <|> do symbol "not"; symbol "("; e <- exprS; symbol ")"; return $ Not e
       <|> do symbol "not "; Not <$> expr'
       <|> do f <- fName; symbol "("; e <- expr'; symbol ")"; return $ Call f [e]
       <|> do f <- fName; symbol "("; symbol ")"; return $ Call f []
       <|> do symbol "["; e <- exprS; symbol "]"; return $ List [e]
       <|> do symbol "["; symbol "]"; return $ List []
       <|> do symbol "("; e<- exprS; symbol ")"; return e

relOp :: Exp -> Parser Exp
relOp e = do symbol "=="; Oper Eq e      <$> exprS 
      <|> do symbol ">";  Oper Greater e <$> exprS
      <|> do symbol "<";  Oper Less e    <$> exprS
      <|> do symbol "!="; e2 <- exprS; relOp (Not (Oper Eq e e2))
      <|> do symbol "<="; e2 <- exprS; relOp (Not (Oper Greater e e2))
      <|> do symbol ">="; e2 <- exprS; relOp (Not (Oper Less e e2))
      <|> do symbol "in "; e2 <- exprS; relOp (Oper In e e2)
      <|> do symbol "not in"; e2 <- exprS; relOp (Not (Oper In e e2))
      <|> do symbol "for "; v <- ident; symbol " in "; e2 <- expr'; relOp (Compr e [CCFor v e2])
      <|> do symbol "if "; e2 <- expr'; relOp (Compr e [CCIf e2])
      <|> return e

addOp :: Exp -> Parser Exp
addOp e = do symbol "+"; Oper Plus e  <$> expr'
      <|> do symbol "-"; Oper Minus e <$> expr' 
      <|> return e

multOp :: Exp -> Parser Exp
multOp e = do symbol "*";  Oper Times e <$> expr' 
       <|> do symbol "//"; Oper Div e   <$> expr'
       <|> do symbol "%";  Oper Mod e   <$> expr'
       <|> return e

negOp :: Exp -> Parser Exp
negOp e = do symbol "not "; negOp $ Not e
      <|> return e

ident :: Parser String
ident = do 
          n <- name 
          if n `elem` reserved then fail "reserved word"
            else return n


-- https://jakewheat.github.io/intro_to_parsing/
name :: Parser String 
name = lexeme $ do i <- letter; is <- many (alphaNum <|> char '_'); return (i:is)

-- name :: Parser String
-- name = do
--     fc <- firstChar
--     rest <- many nonFirstChar
--     return (fc:rest)
--   where
--     firstChar = satisfy (\a -> isLetter a || a == '_')
--     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')




fName :: Parser String
fName = lexeme $
        do 
          f <- many1 $ satisfy isAlpha
          if f `elem` reserved then fail "reserved word"
            else return f




-- TO-DO 
-- (0) if the first char is '-' + space + numConst, then fail
-- -- (1) if first digit is zero, then fail     
numConst :: Parser Int
numConst = lexeme $ do n <- many1 digit; spaces; return $ read n
            <|> do _ <- char '-'; n <- many1 digit; return $ read n*(-1)


-- isNotZero :: Char -> Bool
-- isNotZero c = _ <- lookAhead $ noneOf ['0']

-- numConst :: Parser Int
-- numConst = lexeme $
--             do d <- satisfy isNotZero 
--                ds <- many1 digit
--                return $ read (d:ds) --Multiple digits (check no zero at start)
--             <|>
--             do _ <- char '-'
--                ds <- many1 digit 
--                return $ read ds*(-1) --Negatives
--             <|>
--             do d <- digit
--                return $ read [d] --Single digits

stringConst :: Parser String
stringConst = lexeme $ do s <- many1 letter; spaces; return s

-- fName :: Parser String
-- fName = lexeme $
--         do f <- many1 alphaNum
--            if f `notElem` reserved then return f else fail ""

-- whitespace :: Parser ()
-- whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces ; do _ <- optional comments; do optional tab; return a

symbol :: String -> Parser ()
symbol ps = lexeme $ do _ <- try $ string ps; spaces


reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

comments :: Parser String
comments = do _ <- string "#"; _ <- manyTill anyChar (try newline); return ""


-- fundet på nettet ved ikke endnu hvordan den skal med i koden
-- newstate :: Parser String -> Parser String
-- newstate ((l,c),x:xs) = (newpos, xs)
--                         where 
--                           newpos = case x of 
--                             '\n' -> (l+1,0)
--                             '\t' -> (l,((c,`div` 8)+1)*8)
--                             _    -> (l,c+1)
