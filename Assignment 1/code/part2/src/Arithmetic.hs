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
-- extendEnv v n _r = \vname -> if vname == v then Just n else Nothing
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
  -- evalFull (Sum var from to body) r = sum [evalFull body (extendEnv var x r) | x <- [(evalFull from r)..(evalFull to r)]]
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
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
