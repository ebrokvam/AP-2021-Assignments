-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x) = show x
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")" 
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
showExp _ = error "cannot only show simple expressions"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) = evalSimple x `div` evalSimple y
evalSimple (Pow x y) = evalSimple x ^ evalSimple y
evalSimple _ = error "can only evaluate simple expressions"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r v' = if v == v' then Just n else r v'

evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Add x y) r = evalFull x r + evalFull y r
evalFull (Sub x y) r = evalFull x r - evalFull y r
evalFull (Mul x y) r = evalFull x r * evalFull y r
evalFull (Div x y) r = evalFull x r `div` evalFull y r
evalFull (Pow x y) r = evalFull x r ^ evalFull y r
evalFull (If test yes no) r = if evalFull test r /= 0 then evalFull yes r else evalFull no r
evalFull (Var var) r = case r var of
  Just n -> n
  Nothing -> error "no value bound to variable"
evalFull (Let var def body) r = evalFull body (extendEnv var (evalFull def r) r)
evalFull (Sum var from to body) r = sum [evalFull body (extendEnv var x r) | x <- [(evalFull from r)..(evalFull to r)]]

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = return x
evalErr (Add x y) r = 
  do
  a <- evalErr x r
  b <- evalErr y r
  return $ a + b
evalErr (Sub x y) r = 
  do
  a <- evalErr x r
  b <- evalErr y r
  return $ a - b
evalErr (Mul x y) r = 
  do
  a <- evalErr x r
  b <- evalErr y r
  return $ a * b
evalErr (Div x y) r = 
  do
  a <- evalErr x r
  b <- evalErr y r
  case b of 
    0 -> Left EDivZero
    _ -> return (a `div` b)
evalErr (Pow x y) r = 
  do
  a <- evalErr x r
  b <- evalErr y r
  if b < 0
    then Left ENegPower
    else return (a ^ b)
evalErr (If test yes no) r =
  case evalErr test r of
    Left error -> Left error
    Right x -> 
      if x /= 0
        then evalErr yes r
        else evalErr no r
evalErr (Var v) r =
  case r v of
    Nothing -> Left (EBadVar "v")
    Just x -> Right x
evalErr (Let var def body) r =
  do
  resOfDef <- evalErr def r 
  evalErr body (extendEnv var resOfDef r)
evalErr (Sum var from to body) r =
  do
  a <- evalErr from r
  b <- evalErr to r
  if a > b then return 0 else
    do 
      aa <- evalErr body (extendEnv var a r)
      bb <- evalErr(Sum var (Add (Cst a) (Cst 1)) (Cst b) body) r
      return (aa + bb)
  


-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact e = case e of
  (Cst n) -> show n
  (Add e1 e2) -> showCompact e1 ++ "+" ++ showCompact e2
  (Sub e1 e2) -> showCompact e1 ++ "-" ++ showCompact e2
  (Mul e1 e2) -> showCompact e1 ++ "*" ++ showCompact e2
  (Div e1 e2) -> showCompact e1 ++ "`div`" ++ showCompact e2
  (Pow e1 e2) -> showCompact e1 ++ "^" ++ showCompact e2
  _ -> error "Not allowed expression"


-- showCompact :: Exp -> Bool -> String
-- showCompact e b = case e of
--   (Cst n) -> show n 
--   (Add e1 e2) -> needParenthese e1 b ++ showCompact e1 b ++ "+" ++  showCompact e2 True
--   (Sub e1 e2) -> showCompact e1 b ++ "-" ++ showCompact e2 b
--   (Mul e1 e2) -> showCompact e1 b ++ "*" ++ showCompact e2 b 
--   (Div e1 e2) -> showCompact e1 b ++ "`div`" ++ showCompact e2 b
--   (Pow e1 e2) -> showCompact e1 b ++ "^" ++ showCompact e2 b
--   _ -> error "Not allowed expression"


needParenthese :: Exp -> Bool -> String
needParenthese (Cst _) False = ""
needParenthese e b = if b then "(" ++ needParenthese e b ++ ")" else needParenthese e b


{-
Our current evalErr already implements eager evaluation 
hence why we have re-used it in evalEager
-}
evalEager :: Exp -> Env -> Either ArithError Integer
evalEager (Cst e) _ = return e
evalEager (Var v) r = 
  case r v of
    Nothing -> Left (EBadVar v) 
    Just x -> Right x
evalEager (Add e1 e2) r = 
  do 
  a <- evalEager e1 r
  b <- evalEager e2 r 
  return (a + b)
evalEager (Sub e1 e2) r = 
  do 
  a <- evalEager e1 r
  b <- evalEager e2 r 
  return (a - b)
evalEager (Mul e1 e2) r = 
  do 
  a <- evalEager e1 r
  b <- evalEager e2 r 
  return (a * b)
evalEager (Div e1 e2) r = 
  do 
  a <- evalEager e1 r
  b <- evalEager e2 r 
  case b of 
    0 -> Left EDivZero
    c ->  return (a `div` c)
evalEager (Pow e1 e2) r = 
  do 
  a <- evalEager e1 r
  b <- evalEager e2 r 
  if b < 0 then Left ENegPower else return (a ^ b)
evalEager (If e1 e2 e3) r = 
  do 
  a <- evalEager e1 r
  if a /= 0 then evalEager e2 r else evalEager e3 r
evalEager (Let v e1 e2) r = 
  do
  a <- evalEager e1 r 
  evalEager e2 (extendEnv v a r)
evalEager (Sum v e1 e2 e3) r = 
  do 
  a <- evalEager e1 r 
  b <- evalEager e2 r 
  if a > b then return 0 else 
    do 
    aa <- evalEager e3 (extendEnv v a r)
    bb <- evalEager(Sum v (Add (Cst a) (Cst 1)) (Cst b) e3) r
    return (aa + bb)



{-
Our current evalFull already implements eager evaluation 
hence why we have re-used it in evalLazy with the stylishness 
of evalErr
-}
evalLazy:: Exp -> Env -> Either ArithError Integer
evalLazy (Cst e) _ = return e
evalLazy (Var v) r = 
  case r v of
    Nothing -> Left (EBadVar v) 
    Just x -> Right x
evalLazy (Add e1 e2) r = 
  do 
  a <- evalLazy e1 r
  b <- evalLazy e2 r 
  return (a + b)
evalLazy (Sub e1 e2) r = 
  do 
  a <- evalLazy e1 r
  b <- evalLazy e2 r 
  return (a - b)
evalLazy (Mul e1 e2) r = 
  do 
  a <- evalLazy e1 r
  b <- evalLazy e2 r 
  return (a * b)
evalLazy (Div e1 e2) r = 
  do 
  a <- evalLazy e1 r
  b <- evalLazy e2 r 
  case b of 
    0 -> Left EDivZero
    c ->  return (a `div` c)
evalLazy (Pow e1 e2) r = 
  do 
  a <- evalLazy e1 r
  b <- evalLazy e2 r 
  if b < 0 then Left ENegPower else return (a ^ b)
evalLazy (If e1 e2 e3) r = 
  do 
  a <- evalLazy e1 r
  if a /= 0 then evalLazy e2 r else evalLazy e3 r
evalLazy (Let v e1 e2) r = evalLazy e2 (extendEnv v (evalFull e1 r) r)
evalLazy (Sum v e1 e2 e3) r = 
  do 
  a <- evalLazy e1 r 
  b <- evalLazy e2 r 
  if a > b then return 0 else 
    do 
    aa <- evalLazy e3 (extendEnv v a r)
    bb <- evalLazy(Sum v (Add (Cst a) (Cst 1)) (Cst b) e3) r
    return (aa + bb)