module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) env = return n
eval (Oper op x y) env = oper op <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop :: Expr -> Either String Int
evalTop e = eval e M.empty

{-
Extend the simplifier so that it can perform some more transformations. 
You must implement the following transformations:

(0) Expressions containing operation with the constants 0 and 1 can 
    often be simplified. For instance, an expression e multiplied 
    with 0 can be simplified to 0.

(1) If a let bound variable is not used in the body of a let-expression, 
    then the binding can be eliminated.
-}

simplify :: Expr -> Expr
simplify e =
  case e of
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2) -- changed from (+) to (-).. didn't need quickcheck for that though
    Oper Times (Const 1) (Const c2) -> Const c2      -- don't really simplify as i thought
    Oper Times (Const c1) (Const 1) -> Const c1      -- ditto
    Oper Times (Const 0) _ -> Const 0                -- simplyfies it :) 
    Oper Times _ (Const 0) -> Const 0                -- ditto 
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body ->
      Let v (simplify e) (simplify body)
    _ -> e

{-
simplify (Oper Minus (Oper Plus (Const 5) (Const 6)) (Const 2))
 -> results in: Oper Minus (Const 11) (Const 2)

evalTop (Oper Minus (Oper Plus (Const 5) (Const 6)) (Const 2))
 -> results in: Right 9

simplify (Let "x" (Const 3) (Oper Times (Var "x") (Var "x")))
  -> results in: Let "x" (Const 3) (Oper Times (Var "x") (Var "x"))

evalTop (Let "x" (Const 3) (Oper Times (Var "x") (Var "x")))
  -> results in: Right 9
-}