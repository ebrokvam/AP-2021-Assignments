-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Text.ParserCombinators.Parsec
import Data.Char

parseString :: String -> Either ParseError Program
parseString = parse parseBoa "parse error"


-- REWRITTEN GRAMMER (or at least, the part that was rewritten)
-- Expr   := Expr' NegOp Expr | Expr' Op Expr | Expr'

-- Expr'  := numConst 
--         | stringConst 
--         | ‘None’
--         | ‘True’ 
--         | ‘False’ 
--         | ident
--         | ‘not’ Expr 
--         | ‘(’ Expr ‘)’ 
--         | ident ‘(’ Exprz ‘)’ 
--         | ‘[’ Exprz ‘]’ 
--         | ‘[’ Expr ForClause Clausez ‘]’

-- Op     := '+' | '-' | '*' | "//" | '%' | "==" | "<" | ">" | "in"
-- negOp  := "!=" | "<=" | ">=" | "not in"

parseBoa :: Parser Program
parseBoa = do spaces; p <- stmts; eof; return p

stmts :: Parser [Stmt]
stmts = try (do s <- stmt; symbol ";"; ss <- stmts; return (s:ss))
    <|> do s <- stmt; return [s]

stmt :: Parser Stmt
stmt = try (do i <- ident; symbol "="; e <- expr; return $ SDef i e)
    <|> do e <- expr; return (SExp e)

expr :: Parser Exp
expr = lexeme $ try (do e <- expr'; o <- negOp; e' <- expr; return $ Not $ Oper o e e')
    <|> try (do e <- expr'; o <- op; e' <- expr; return $ Oper o e e')
    <|> do expr'

expr' :: Parser Exp
expr' = lexeme $ do n <- numConst; return (Const (IntVal n))
    <|> do symbol "\'"; s <- stringConst; return (Const (StringVal s))
    <|> do symbol "None"; return (Const NoneVal)
    <|> do symbol "True"; return (Const TrueVal)
    <|> do symbol "False"; return (Const FalseVal)
    <|> try (do string "not"; spaces; e <- expr; return (Not e))
    <|> try (do i <- ident; symbol "("; es <- exprz; symbol ")"; return (Call i es))
    <|> do i <- ident; return (Var i)
    <|> do symbol "("; e <- expr; symbol ")"; return e
    <|> try (do symbol "["; es <- exprz; symbol "]"; return (List es))
    <|> do symbol "["; e <- expr; fc <- forClause; cs <- clausez; symbol "]"; return (Compr e (fc:cs))

op :: Parser Op
op = lexeme $ do char '+'; return Plus
    <|> do char '-'; return Minus
    <|> do char '*'; return Times
    <|> do string "//"; return Div
    <|> do char '%'; return Mod
    <|> do string "=="; return Eq;
    <|> do char '<'; return Less
    <|> do char '>'; return Greater
    <|> do string "in"; many1 space; return In

negOp :: Parser Op
negOp = lexeme $ do string "!="; return Eq
    <|> do string "<="; return Greater
    <|> do string ">="; return Less
    <|> do string "not in"; return In

forClause :: Parser CClause
forClause = do string "for"; many1 space; i <- ident; string "in"; many1 space; e <- expr; return (CCFor i e)

ifClause :: Parser CClause
ifClause = do string "if"; many1 space; e <- expr; spaces; return (CCIf e)

clausez :: Parser [CClause]
clausez = lexeme $ try (do lookAhead (char ']'); return [])
    <|> do fc <- forClause; cs <- clausez; return (fc:cs)
    <|> do ic <- ifClause; cs <- clausez; return (ic:cs)

exprz :: Parser [Exp]
exprz = try (do lookAhead (oneOf ")]"); return [])
    <|> do exprs

exprs :: Parser [Exp]
exprs = try (do e <- expr; symbol ","; es <- exprs; return (e:es))
    <|> do e <- expr; return [e]

ident :: Parser String
ident = lexeme $ do lookAhead (noneOf "0123456789"); many1 isIdent;

isIdent :: Parser Char
isIdent = (isKeyword >> fail "reserved keyword")
    <|> satisfy (\c -> isAlphaNum c || c == '_')

isKeyword :: Parser ()
isKeyword = try (do string "None"; spaces; oneOf "=("; return ())
    <|> try (do string "True"; spaces; oneOf "=("; return ())
    <|> try (do string "False"; spaces; oneOf "=("; return ())
    <|> try (do string "for"; spaces; oneOf "=("; return ())
    <|> try (do string "if"; spaces; oneOf "=("; return ())
    <|> try (do string "in"; spaces; oneOf "=("; return ())
    <|> try (do string "not"; spaces; oneOf "=("; return ())

numConst :: Parser Int
numConst = lexeme $ do string "-"; ds <- getDigits; return (read ("-"++ds))
    <|> do ds <- getDigits; return (read ds)

getDigits :: Parser String
getDigits = do char '0'; return "0"
    <|> do lookAhead (noneOf "0"); many1 digit;

stringConst :: Parser String
stringConst = do manyTill isStringConst (char '\'')

isStringConst :: Parser Char
isStringConst = newline
    <|> (string "\\\\" >> return '\\') 
    <|> (string "\\\'" >> return '\'') 
    <|> (char '\\' >> fail "single backslash")
    <|> do lookAhead $ satisfy isPrint; satisfy isAscii

comment :: Parser ()
comment = try (do char '#'; skipMany (noneOf "\\"); char '\n'; return ())
    <|> do char '#'; skipMany (noneOf "\\"); eof

lexeme :: Parser a -> Parser a
lexeme p = try (do a <- p; comment; return a)
    <|> do a <- p; spaces; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return () 