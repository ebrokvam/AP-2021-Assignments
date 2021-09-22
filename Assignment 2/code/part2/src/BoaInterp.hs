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

instance Monad Comp where
  return a = Comp $ \_r -> (Right a, [])
  m >>= f = Comp $ \r ->
    case runComp m r of
      (Left re, s') -> (Left re, s')
      (Right a, s') ->
        case runComp (f a) r of
          (Left re, s'') -> (Left re, s' ++ s'')
          (Right b, s'') -> (Right b, s' ++ s'')

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp $ \_r -> (Left re, [])

look :: VName -> Comp Value
look x = Comp $ \r -> 
  case lookup x r of
    Nothing -> runComp (abort (EBadVar x)) r
    Just value -> (Right value, [])

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp $ \r -> runComp m ((x, v):r)

output :: String -> Comp ()
output s = Comp $ \_r -> (Right (), [s])

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal e1) (IntVal e2) = Right (IntVal (e1 + e2))
operate Minus (IntVal e1) (IntVal e2) = Right (IntVal (e1 - e2))
operate Times (IntVal e1) (IntVal e2) = Right (IntVal (e1 * e2))
operate Div (IntVal e1) (IntVal e2) = if e2 == 0 then Left "attempted division by zero" else Right (IntVal (e1 `div` e2))
operate Mod (IntVal e1) (IntVal e2) = if e2 == 0 then Left "attempted division by zero" else Right (IntVal (e1 `mod` e2))
operate Eq e1 e2 = if e1 == e2 then Right TrueVal else Right FalseVal
operate Less (IntVal e1) (IntVal e2) = if e1 < e2 then Right TrueVal else Right FalseVal
operate Greater (IntVal e1) (IntVal e2) = if e1 > e2 then Right TrueVal else Right FalseVal
-- has to compare with Eq, hence recursively comparing each element
operate In _ (ListVal []) = Right FalseVal
operate In e1 (ListVal (x:xs)) = if (operate Eq e1 x) == Right TrueVal then Right TrueVal else operate In e1 (ListVal xs)
operate _ _ _ = Left "invalid value type for operation"

apply :: FName -> [Value] -> Comp Value
apply "range" vs = compRange vs
apply "print" vs = compPrint vs
apply f _ = abort (EBadFun f)

compRange :: [Value] -> Comp Value
compRange vs = --TODO: remove end element
  case vs of
    [(IntVal start), (IntVal end), (IntVal stepSize)] ->
      if stepSize == 0
        then abort (EBadArg "step size cannot be zero in range function")
        else if start < end && stepSize > 0
          then return (ListVal (map IntVal [start,(start+stepSize)..(end-1)]))
          else if start > end && stepSize < 0
            then return (ListVal (map IntVal [start,(start+stepSize)..(end+1)]))
            else return (ListVal [])
    [(IntVal start), (IntVal end)] ->
      if start < end
        then return (ListVal (map IntVal [start..end-1]))
        else return (ListVal (map IntVal [start..end+1]))
    [(IntVal end)] ->
      return (ListVal (map IntVal [0..(end-1)]))
    _ ->
      abort (EBadArg "incorrect list size or value types for range function")

compPrint :: [Value] -> Comp Value
compPrint vs = 
  do
  output (convertString vs)
  return NoneVal

convertString :: [Value] -> String
convertString [] = ""
convertString (v:vs)=
  case v of
    NoneVal -> "None" ++ getSpace (v:vs) ++ convertString vs
    TrueVal -> "True" ++ getSpace (v:vs) ++ convertString vs
    FalseVal -> "False" ++ getSpace (v:vs) ++ convertString vs
    IntVal x -> show x ++ getSpace (v:vs) ++ convertString vs
    StringVal s -> s ++ getSpace (v:vs) ++ convertString vs
    ListVal ls -> convertList ls ++ getSpace (v:vs) ++ convertString vs

getSpace :: [Value] -> String
getSpace vs = if length vs == 1 then "" else " "

convertList :: [Value] -> String
convertList vs = "[" ++ convertListElement vs ++ "]"

-- same as convertString but with getComma
convertListElement :: [Value] -> String
convertListElement [] = ""
convertListElement (v:vs) =
  case v of
    NoneVal -> "None" ++ getComma (v:vs) ++ convertListElement vs
    TrueVal -> "True" ++ getComma (v:vs) ++ convertListElement vs
    FalseVal -> "False" ++ getComma (v:vs) ++ convertListElement vs
    IntVal x -> show x ++ getComma (v:vs) ++ convertListElement vs
    StringVal s -> s ++ getComma (v:vs) ++ convertListElement vs
    ListVal ls -> convertList ls ++ getComma (v:vs) ++ convertListElement vs

getComma :: [Value] -> String
getComma vs = if length vs == 1 then "" else ", "

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var x) = look x
eval (Oper o e1 e2) =
  do
  res1 <- eval e1
  res2 <- eval e2
  case operate o res1 res2 of
    Left s -> abort (EBadArg s)
    Right res3 -> return res3
eval (Not e) =
  do
  a <- eval e
  if truthy a then return FalseVal else return TrueVal
eval (Call f en) =
  do
  res <- mapM eval en
  apply f res
eval (List en) = 
  do
  res <- (mapM eval en)
  return (ListVal res)
-- eval (Compr _ [CCFor x e]) = 
--   case eval e of 
--     (ListVal res) -> withBinding x res --wtfffff
--     _ -> abort (EBadArg "I am so goddamn confused") --TODO: don't leave this message
-- eval (Compr _ [CCIf e]) = 
--   if truthy (eval e) 
--     then abort (EBadArg "I am so goddamn confused")
--     else --TODO: don't leave this message
eval (Compr e0 (cc:ccs)) = 
  case cc of
    (CCFor x e) -> 
      do
      res <- eval e
      case res of
        (ListVal res) -> withBinding x (ListVal res) (eval (Compr e0 ccs))
        _ -> abort (EBadArg "I am so goddamn confused") --TODO: don't leave this message
    (CCIf e) ->
      do
      res <- eval e
      if truthy res
        then abort (EBadArg "I am so goddamn confused")
        else (eval (Compr e0 ccs))

exec :: Program -> Comp ()
exec [SDef _ e] =
  do
  _res <- eval e
  return ()
exec [SExp e] =
  do
  _res <- eval e
  return ()
exec (sm:sms) = 
  case sm of
    (SDef x e) -> 
      do
      res <- eval e
      withBinding x (res) (exec sms)
    (SExp e) ->
      do
      _res <- eval e
      (exec sms)

execute :: Program -> ([String], Maybe RunError)
-- execute (sm:sms) =
--   \r ->
--   case sm of
--     (SDef x e) ->
--       do
--       res <- eval e
--       (_, s) runComp (execute sms) r
--       case res of
--         (Left e) -> (s, Just e)
--         (Right _) -> (s, Nothing)
--     (SExp e) ->
execute = undefined