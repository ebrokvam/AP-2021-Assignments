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

------ Operations of the monad ------

{- 
abort re is used for signaling the runtime error re 
-}
abort :: RunError -> Comp a
abort re = Comp $ \_r -> (Left re, [])

{- 
look x returns the current binding of the variable x 
(or signals an EBadVar x error if x is unbound) 
-}
look :: VName -> Comp Value
look x = Comp $ \r -> 
  case lookup x r of
    Nothing -> runComp (abort (EBadVar x)) r
    Just value -> (Right value, [])

{- 
withBinding x v m runs the computation m with x 
bound to v, in addition to any other current bindings
-}
withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp $ \r -> runComp m ((x, v):r)

{- 
output s appends the line s to the output list. 
s should not include a trailing newline character 
(unless the line arises from printing a string value 
that itself contains an embedded newline)
-}
output :: String -> Comp ()
output s = Comp $ \_r -> (Right (), [s])



------ Helper functions for interpreter ------

{- 
truthy v simply determines whether the value v 
represents truth or falsehood, as previously specified
-}
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True


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
operate In e1 (ListVal (x:xs)) = if operate Eq e1 x == Right TrueVal then Right TrueVal else operate In e1 (ListVal xs)
operate _ _ _ = Left "invalid value type for operation"


{-
apply f [v1,...,vn] applies the built-in function f to the 
(already evaluated) argument tuple v1, ..., vn, possibly 
signaling an error if f is not a valid function name (EBadFun), 
or if the arguments are not valid for the function (EBadArg)
-}
apply :: FName -> [Value] -> Comp Value
apply "range" vs = compRange vs
apply "print" vs = compPrint vs
apply f _ = abort (EBadFun f)

------ Main functions of interpreter ------

{-
eval e is the computation that evaluates the expression e 
in the current environment and returns its value
-}
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
  res <- mapM eval en
  return (ListVal res)
eval (Compr en (c:cs)) =
  case c of 
    CCFor x e1 -> do
      e2 <- eval e1
      case e2 of 
        ListVal lst -> do
          vals <- mapM (\v -> withBinding x v (eval (Compr en cs))) lst
          return $ ListVal (concatMap (\(ListVal val) -> val) vals)
        _ -> abort $ EBadArg "invalid argument: CCFor expected list"
    CCIf e1 -> do
      e2 <- eval e1
      if truthy e2 then eval (Compr en cs) else return $ ListVal []
eval (Compr en []) =
  do 
  e1 <- eval en
  return $ ListVal [e1]


-- WHAT WE TRIED TO DO FOR COMPR

-- eval (Compr _ [CCFor x e]) = 
--   case eval e of 
--     (ListVal res) -> withBinding x res
--     _ -> abort (EBadArg "what")
-- eval (Compr _ [CCIf e]) = 
--   if truthy (eval e) 
--     then abort (EBadArg "failed guard")
--     else
-- eval (Compr e0 (cc:ccs)) = 
--   case cc of
--     (CCFor x e) -> 
--       do
--       res <- eval e
--       case res of
--         (ListVal res) -> withBinding x (ListVal res) (eval (Compr e0 ccs))
--         _ -> abort (EBadArg "what")
--     (CCIf e) ->
--       do
--       res <- eval e
--       if truthy res
--         then abort (EBadArg "failed guard")
--         else (eval (Compr e0 ccs))


{-
exec p is the computation arising from executing the program 
(or program fragment) p, with no nominal return value, but 
with any side effects in p still taking place in the computation

SDef x e evaluates e to a value, and binds x to that value for 
the remaining statements in the sequence

SExp e just evaluates e and discards the result value
-}
exec :: Program -> Comp ()
exec [] = return ()
exec (sm:sms) = 
  case sm of
    (SDef x e) -> 
      do
      res <- eval e
      withBinding x res (exec sms)
    (SExp e) ->
      do
      _res <- eval e
      exec sms

{-
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



------ The two built-in functions of BOA + auxillary functions ------

-- aux functions for apply
{-
compRange computes the range as foretold by the assignment text
-}
compRange :: [Value] -> Comp Value
compRange vs =
  case vs of
    [IntVal start, IntVal end, IntVal stepSize] ->
      if stepSize == 0
        then abort (EBadArg "step size cannot be zero in range function")
        else if start < end && stepSize > 0
          then return (ListVal (map IntVal [start,(start+stepSize)..(end-1)]))
          else if start > end && stepSize < 0
            then return (ListVal (map IntVal [start,(start+stepSize)..(end+1)]))
            else return (ListVal [])
    [IntVal start, IntVal end] ->
      if start < end
        then return (ListVal (map IntVal [start..end-1]))
        else return (ListVal (map IntVal [start..end+1]))
    [IntVal end] ->
      return (ListVal (map IntVal [0..(end-1)]))
    _ ->
      abort (EBadArg "incorrect list size or value types for range function")


{-
compPrint computes the range as foretold by the assignment text
uses following functions to function properly
 - convertString
 - getSpace
 - convertList
 - convertListElement
 - getComma

-}
compPrint :: [Value] -> Comp Value
compPrint vs = 
  do
  output (convertString vs)
  return NoneVal

{-
takes the values given by compPrint and prints the value;
main print function calls on getSpace to, well, get space
and convertList
-}
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

{-
returns either a space or none based on the length of the
given value
-}
getSpace :: [Value] -> String
getSpace vs = if length vs == 1 then "" else " "

{-
returns "[" "]" when evoked
-}
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

{-
same semantic as getSpace here it returns a ","
 -}
getComma :: [Value] -> String
getComma vs = if length vs == 1 then "" else ", "
