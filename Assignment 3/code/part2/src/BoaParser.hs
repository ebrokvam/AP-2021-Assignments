-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Text.ParserCombinators.Parsec.Prim()
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
stmtS = do s <- stmt; return [s]
       <|> do s' <- stmt; symbol ";"; s'' <- stmtS; return (s':s'')

stmt :: Parser Stmt
stmt = do v <- ident; symbol "="; SDef v <$> exprS
      <|> do SExp <$> exprS


exprS :: Parser Exp
exprS = do e <- expr; eof; return e
       <|> do e' <- expr; symbol ","; e'' <- exprS; return $ List [e',e'']

expr :: Parser Exp
expr = do e <- term'; relOp e
-- expr = term' `chainl1` relOp

term' :: Parser Exp
term' = do t' <- term; addOp t'

term :: Parser Exp
term = do t <- factor; multOp t

factor :: Parser Exp
factor = Const . IntVal <$> numConst
       <|> Var <$> ident
       <|> do symbol "("; f <- exprS; symbol ")"; return f
       -- <|> do symbol "'"; f <- stringConst; symbol "'"; return $ Const (StringVal f)

{-
overvej at gøre noget ligende 
-- https://stackoverflow.com/questions/16367150/how-to-make-the-parsec-chainl1-function-follow-operator-precedence-rules

dvs. split alle Op op i hver deres foo og på den måde gøre brug af `chainl1`

-}
-- -- relOp :: Parser (Exp -> Exp -> Exp)
-- relOp   = do symbol "=="; Oper Eq 
--       <|> do symbol ">";  Oper Greater
--       <|> do symbol "<";  Oper Less 
--       <|> do symbol "in"; Oper In 
--       -- <|> do symbol "not"; Not  

-- addOp :: Exp -> Parser Exp
-- addOp e = do symbol "+"; Oper Plus e  <$> exprS
--       <|> do symbol "-"; Oper Minus e <$> exprS 

-- multOp :: Exp -> Parser Exp
-- multOp e = do symbol "*";  Oper Times e <$> exprS 
--        <|> do symbol "//"; Oper Div e   <$> exprS
--        <|> do symbol "%";  Oper Mod e   <$> exprS


relOp :: Exp -> Parser Exp
relOp e = do symbol "=="; Oper Eq e      <$> exprS 
      <|> do symbol ">";  Oper Greater e <$> exprS
      <|> do symbol "<";  Oper Less e    <$> exprS
      <|> do symbol "in"; Oper In e      <$> exprS
      <|> do symbol "not"; Not           <$> exprS

addOp :: Exp -> Parser Exp
addOp e = do symbol "+"; Oper Plus e  <$> exprS
      <|> do symbol "-"; Oper Minus e <$> exprS 

multOp :: Exp -> Parser Exp
multOp e = do symbol "*";  Oper Times e <$> exprS 
       <|> do symbol "//"; Oper Div e   <$> exprS
       <|> do symbol "%";  Oper Mod e   <$> exprS



ident :: Parser String
ident = lexeme $ 
        do 
          i <- letter
          is <- many (alphaNum <|> char '_')
          if (i:is) `elem` reserved then fail "reserved word"
            else return (i:is)

numConst :: Parser Int
numConst = lexeme $ do n <- many1 digit; spaces; return $ read n

stringConst :: Parser String
stringConst = lexeme $ do s <- many1 letter; spaces; return $ read s


-- whitespace :: Parser ()
-- whitespace = do _ <- space; _ <- newline; _ <- tab; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; spaces; return a

symbol :: String -> Parser ()
symbol ps = lexeme $ do _ <- try $ string ps; spaces


reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]
