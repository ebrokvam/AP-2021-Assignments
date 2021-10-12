module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E

-- Make Expr an instance of the type-class Arbitrary 
-- a generator for arithmetic expressions
instance Arbitrary Expr where
   arbitrary = expr
   shrink (Const n) = map Const $ shrink n
   shrink (Oper op x y) = [x, y] ++ [Oper op x' y' | (x', y') <- shrink (x, y)]
   -- shrink (Let v e body) = 

-- use this to find the bug in simplifier 
prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.simplify(x) === E.simplify(x)

expr = sized exprN 
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary, 
                  Oper Plus <$> subexpr <*> subexpr,
                  Oper Minus <$> subexpr <*> subexpr,
                  Oper Times <$> subexpr <*> subexpr]
  where subexpr = exprN (n `div` 2)
