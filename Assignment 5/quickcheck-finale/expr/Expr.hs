{-
Example code produced for Advanced Programming lecture.

Main purpose is to show how to generate recursive types.
-}
import Test.QuickCheck

data Expr = Con Int
          | Add Expr Expr
          | Minus Expr Expr
          deriving (Eq, Show, Read, Ord)

eval :: Expr -> Int
eval (Con n) = n
eval (Add x y) = eval x + eval y
eval (Minus x y) = eval x - eval y

-- Property: Add is commutative
prop_com_add x y = eval (Add x y) == eval (Add y x)

prop_assoc_minus x y z = eval(Minus x (Minus y z)) === eval(Minus (Minus x y) z)

expr =  oneof [ fmap Con arbitrary
              , do x <- expr
                   y <- expr
                   return $ Add x y
              ]

--instance Arbitrary Expr where
--  arbitrary = expr

-- -- Take 2: using sized generators and shrinking
instance Arbitrary Expr where
   arbitrary = expr2

   shrink (Con n) = map Con $ shrink n
   shrink (Add e1 e2) =
     -- shrink to subterms
     [e1, e2] ++
     -- recursively shrink subterms
     [Add e1' e2' | (e1', e2') <- shrink (e1, e2)]
   shrink (Minus e1 e2) =
     -- shrink to subterms
     [e1, e2] ++
     -- recursively shrink subterms
     [Minus e1' e2' | (e1', e2') <- shrink (e1, e2)]


expr2 = sized exprN
exprN 0 = fmap Con arbitrary
exprN n = oneof [ fmap Con arbitrary
                , Add <$> subexpr <*> subexpr
                , Minus <$> subexpr <*> subexpr
                ]
  where subexpr = exprN (n `div` 2)
