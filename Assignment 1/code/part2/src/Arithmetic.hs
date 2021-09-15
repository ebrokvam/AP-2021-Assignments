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
import Data.Maybe (fromMaybe)

showExp :: Exp -> String
showExp (Cst e) = show e
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")" 
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "`div`" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _ = error "cannot only show simple expressions"

evalSimple :: Exp -> Integer
evalSimple (Cst e) = e
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) = evalSimple e1 `div` evalSimple e2
evalSimple (Pow e1 e2) = evalSimple e1 ^ evalSimple e2
evalSimple _ = error "can only evaluate simple expressions"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r v' = if v == v' then Just n else r v'

evalFull :: Exp -> Env -> Integer
evalFull (Cst e) _ = e
evalFull (Add e1 e2) r = evalFull e1 r + evalFull e2 r
evalFull (Sub e1 e2) r = evalFull e1 r - evalFull e2 r
evalFull (Mul e1 e2) r = evalFull e1 r * evalFull e2 r
evalFull (Div e1 e2) r = evalFull e1 r `div` evalFull e2 r
evalFull (Pow e1 e2) r = evalFull e1 r ^ evalFull e2 r
evalFull (If e1 e2 e3) r = if evalFull e1 r /= 0 then evalFull e2 r else evalFull e3 r
evalFull (Var v) r = fromMaybe (error "no value bound to variable") (r v)
evalFull (Let v e1 e2) r = evalFull e2 (extendEnv v (evalFull e1 r) r)
evalFull (Sum v e1 e2 e3) r = sum [evalFull e3 (extendEnv v x r) | x <- [(evalFull e1 r)..(evalFull e2 r)]]

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst e) _ = return e
evalErr (Var v) r = 
  case r v of
    Nothing -> Left (EBadVar v) 
    Just x -> Right x
evalErr (Add e1 e2) r = 
  do 
  a <- evalErr e1 r
  b <- evalErr e2 r 
  return (a + b)
evalErr (Sub e1 e2) r = 
  do 
  a <- evalErr e1 r
  b <- evalErr e2 r 
  return (a - b)
evalErr (Mul e1 e2) r = 
  do 
  a <- evalErr e1 r
  b <- evalErr e2 r 
  return (a * b)
evalErr (Div e1 e2) r = 
  do 
  a <- evalErr e1 r
  b <- evalErr e2 r 
  case b of 
    0 -> Left EDivZero
    c ->  return (a `div` c)
evalErr (Pow e1 e2) r = 
  do 
  a <- evalErr e1 r
  b <- evalErr e2 r 
  if b < 0 then Left ENegPower else return (a ^ b)
evalErr (If e1 e2 e3) r = 
  do 
  a <- evalErr e1 r
  if a /= 0 then evalErr e2 r else evalErr e3 r
evalErr (Let v e1 e2) r = 
  do
  a <- evalErr e1 r 
  evalErr e2 (extendEnv v a r)
evalErr (Sum v e1 e2 e3) r = 
  do 
  a <- evalErr e1 r 
  b <- evalErr e2 r 
  if a > b then return 0 else 
    do 
    aa <- evalErr e3 (extendEnv v a r)
    bb <- evalErr(Sum v (Add (Cst a) (Cst 1)) (Cst b) e3) r
    return (aa + bb)
  


-- optional parts

showCompact :: Exp -> String
showCompact e = case e of
  (Cst n) -> show n
  (Add e1 e2) -> showCompact e1 ++ "+" ++ showCompact e2
  (Sub e1 e2) -> showCompact e1 ++ "-" ++ showCompact e2
  (Mul e1 e2) -> showCompact e1 ++ "*" ++ showCompact e2
  (Div e1 e2) -> showCompact e1 ++ "`div`" ++ showCompact e2
  (Pow e1 e2) -> showCompact e1 ++ "^" ++ showCompact e2
  _ -> error "Not allowed expression"


 -----------  *Pow (Div (Cst 4) (Cst 0)) (Cst 0):      1

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