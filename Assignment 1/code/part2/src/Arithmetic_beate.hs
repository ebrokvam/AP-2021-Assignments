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


{-
Here the base case would be if the Exp is simply an integer and since 
its a string we use the buildt-in function show (Show a => a -> String)
The rest of the cases are more or less the same except from the unary operation. 
Since we can't be sure what Exp is, we recursively run showExp on Exp until we 
get to the base case. Instead of '/' we are using `div` for division
-}
showExp :: Exp -> String
showExp (Cst e) = "(" ++ show e ++ ")"
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "`div`" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _ = error "Not allowed expression"


{-
As with the function above, showExp, if the Exp is an integer it'll 
show the interger.  The rest of the functions works in the same 
fashion as above. Since we don't know what the Exp we recursivly 
check ut Exp with evalSimple until we get to an integer. 

Here `seq` is used to force Haskell to evaluate e1 and then evaluate both e1 and e2. 
This is to make sure that the precedence of computations are done from inside out. 
Furthermore, it also ensures that the expression "0 `div`0 ^ 0" results in an error.
We know that this work since `seq` ensure that if we are evaluating the second argument
then we must also evaluate the first argument.
-}
evalSimple :: Exp -> Integer
evalSimple (Cst e) = e
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2 
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2 
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2 
evalSimple (Div e1 e2) = if evalSimple e2 == 0 then error "divsion by zero" else evalSimple e1 `seq`  (evalSimple e1 `div`evalSimple e2)
evalSimple (Pow e1 e2) = if evalSimple e2 < 0 then error "power to negative number" else evalSimple e1 `seq` (evalSimple e1 ^ evalSimple e2)
evalSimple _ = error "Not allowed expression"


{-
This function takes three arguments where the last one is split into two, 
such that, we can compare the new var to an existing var to check if it already exits
in the environment or not. 
-}
extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r v' = if v == v' then Just n else r v'


{- 
Same as with evalSimple, in principle, but with some added functionalities. 
Those are the If, Let, Sum expressions. In the let expression we make use 
of the function extendEnv to init an v in a env. Sum we evaluate e3 and 
then add the evaluation of sum where we add e1 with 1 
beause we know that in this specific case because each step is only one  
-}
evalFull :: Exp -> Env -> Integer
evalFull (Cst e) _ = e
evalFull (Var v) r = fromMaybe (error "unbound variable") (r v)
    -- case r v of 
    --     Nothing -> error "unbound variable"
    --     Just x -> x
evalFull (Add e1 e2) r = evalFull e1 r + evalFull e2 r
evalFull (Sub e1 e2) r = evalFull e1 r - evalFull e2 r 
evalFull (Mul e1 e2) r = evalFull e1 r * evalFull e2 r 
evalFull (Div e1 e2) r = if evalFull e2 r == 0 then error "divsion by zero" else evalFull e1 r `seq` (evalFull e1 r `div` evalFull e2 r)
evalFull (Pow e1 e2) r = if evalFull e2 r < 0 then error "power to negative number" else evalFull e1 r `seq` (evalFull e1 r ^ evalFull e2 r)
evalFull (If e1 e2 e3) r = if evalFull e1 r /= 0 then evalFull e2 r else evalFull e3 r
evalFull (Let v e1 e2) r = evalFull e2 (extendEnv v (evalFull e1 r) r)
evalFull (Sum v e1 e2 e3) r = if evalFull e1 r > evalFull e2 r then 0 else evalFull e3 (extendEnv v (evalFull e1 r) r) + evalFull (Sum v (Add e1 (Cst 1)) e2 e3) r


{-
Here we are using monads to handle our error-handling 
As can be seen we split up the cases to evaluate them 
individually and then combine them. 

For (Cst e) we simply just return it 
For (Var v) we check if it's bound or not by 
    looking at the def of initEnv and extencEnv


For Add, Sub, and Mul the method are more less 
the same unpack up the expression, then compute,
and then pack and return

Div and Mul are almost like the others
except we have some conditions that needs to 
be met. 

The If, Let and Sum expression are almost 
identical with the ones in evalFull.
But as with Add & Co. we unpack
the expressions values, compute what needs
to be computed, and then we pack and return it. 
-}
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
--   (Add e1 e2) -> showCompact e1 b ++ "+" ++ needParenthese e2 b ++ showCompact e2 True
--   (Sub e1 e2) -> showCompact e1 b ++ "-" ++ showCompact e2 b
--   (Mul e1 e2) -> showCompact e1 b ++ "*" ++ showCompact e2 b 
--   (Div e1 e2) -> showCompact e1 b ++ "`div`" ++ showCompact e2 b
--   (Pow e1 e2) -> showCompact e1 b ++ "^" ++ showCompact e2 b
--   _ -> error "Not allowed expression"

-- needParenthese :: Exp -> Bool -> String
-- needParenthese (Cst _) False = ""
-- needParenthese e b = if b then "(" ++ needParenthese e b ++ ")" else needParenthese e b


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



evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
-- evalLazy (Let v e1 e2) r = 