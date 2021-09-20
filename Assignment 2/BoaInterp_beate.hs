-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }


{-
Our first attempt on the monad comp. if it still has left/right
then it means we didn't get around to exploit the either monad...

-}
instance Monad Comp where
  return a = Comp $ const (Right a, mempty)
  m >>= f = Comp  $ \env -> 
    case runComp m env of
      (Left err, s) -> (Left err, s)
      (Right a, s) -> 
        case runComp (f a) env of
          (Left err, s') -> (Left err, s <> s')
          (Right b, s') -> (Right b, s <> s')


-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap


-- Operations of the monad

{- 
abort re is used for signaling the runtime error re 
-}
abort :: RunError -> Comp a
abort re = Comp (const (Left re, mempty))


{- 
look x returns the current binding of the variable x 
(or signals an EBadVar x error if x is unbound) 
-}
look :: VName -> Comp Value
look x = Comp (\env -> 
  case lookup x env of 
    Nothing -> (Left (EBadVar x), [])
    Just z -> (Right z, [])) 

{- 
withBinding x v m runs the computation m with x 
bound to v, in addition to any other current bindings
-}
withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp $ \env -> runComp m ((x, v):env)


{- 
output s appends the line s to the output list. 
s should not include a trailing newline character 
(unless the line arises from printing a string value 
that itself contains an embedded newline)
-}
output :: String -> Comp ()
output s = Comp (const (Right (), [s]))


-- Helper functions for interpreter

{- 
truthy v simply determines whether the value v 
represents truth or falsehood, as previously specified
-}
truthy :: Value -> Bool
truthy v = case v of 
  NoneVal       -> False
  TrueVal       -> True
  FalseVal      -> False
  IntVal _      -> True    -- should be re-done
  IntVal 0      -> False 
  StringVal ""  -> False
  StringVal _   -> True
  ListVal []    -> False
  ListVal (_:_) -> True



{- 
operate o v1 v2 applies the operator o to the arguments 
v1 and v2, returning either the resulting value, or an error 
message if one or both arguments are inappropriate for the 
operation

operate has changed visual from last year; only In was a true
copy-paste from last year w.r.g. to the element search...
much simpler than what I was about to venture into -.-
-}

operate :: Op -> Value -> Value -> Either String Value
operate o v1 v2 = case (o, v1, v2) of
  (Plus, IntVal v', IntVal v'')  -> Right $ IntVal (v' + v'')
  (Minus, IntVal v', IntVal v'') -> Right $ IntVal (v' - v'')
  (Times, IntVal v', IntVal v'') -> Right $ IntVal (v' * v'')
  (Div, IntVal v', IntVal v'') -> 
    if v'' < 0 then Left "div by zero is verboten" else
      Right $ IntVal (v' `div` v'') 
  (Mod, IntVal v', IntVal v'') -> 
    if v'' < 0 then Left "mod by zero is verboten" else
      Right $ IntVal (v' `mod` v'')
  (Eq, v', v'')   -> 
    if v' /= v'' then Right FalseVal  else
      Right TrueVal
  (Less, IntVal v', IntVal v'') -> 
    if v' > v'' then Right FalseVal else
      Right TrueVal
  (Greater, IntVal v', IntVal v'') -> 
    if v' < v'' then Right FalseVal else
      Right TrueVal
  (In, v', ListVal v'') -> -- only In is taken from last year 
    if Right TrueVal `elem` [operate Eq v' h | h <- v''] then Right TrueVal
      else Right FalseVal
      {-
      `elem` [operate Eq v' h | h <- v''] for each element in this list,
      compare v' with h, where h is bound to v''
      -}
  _ -> Left "invalid argument"


{-
apply f [v1,...,vn] applies the built-in function f to the 
(already evaluated) argument tuple v1, ..., vn, possibly 
signaling an error if f is not a valid function name (EBadFun), 
or if the arguments are not valid for the function (EBadArg)
-}
apply :: FName -> [Value] -> Comp Value
apply = undefined


-- Main functions of interpreter

{-
eval e is the computation that evaluates the expression e 
in the current environment and returns its value
-}
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var x) = look x
eval (Oper o v' v'') = 
  do 
    a <- eval v'
    b <- eval v'' 
    case operate o a b of 
      Left err -> abort $ EBadArg err
      Right res -> return res
-- eval (Not e) = 
--   do 
--     y <- eval e
--     return $ not $ truthy y
-- 0. eval the argument tuple, 1. call the functions from hell
-- eval (Call fn [e]) = 
--   do 
--     z <- foldl eval e [e]
--     return $ apply fn z
-- eval (List [e]) = 
-- eval (Compr e [cc]) = 


{-
exec p is the computation arising from executing the program 
(or program fragment) p, with no nominal return value, but 
with any side effects in p still taking place in the computation

SDef x e evaluates e to a value, and binds x to that value for 
the remaining statements in the sequence

SExp e just evaluates e and discards the result value
-}
exec :: Program -> Comp ()
exec [] = Comp (const (Right (), mempty))
exec (p:ps) = case p:ps of
  [SDef x e] -> do v <- eval e; withBinding x v $ exec ps
  [SExp e] -> do _ <- eval e; exec ps
  [] -> Comp (const (Right (), mempty))



{-
output :: String -> Comp ()
output s = Comp (const (Right (), [s]))

    Nothing -> (Left (EBadVar x), [])
    Just z -> (Right z, [])) 

execute p explicitly returns the list of output lines, and the
error message (if relevant) resulting from executing p in the 
initial environment, which contains no variable bindings
-}
execute :: Program -> ([String], Maybe RunError)
execute p = 
  let (u,v) = runComp (exec p) [] in 
    case u of 
      Right _ -> (v, Nothing)
      Left err -> (v, Just err)