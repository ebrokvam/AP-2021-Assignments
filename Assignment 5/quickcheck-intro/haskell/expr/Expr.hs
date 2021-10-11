{-
Example code produced for Advanced Programming lecture.

Main purpose is to show how to generate recursive types.
-}
import Test.QuickCheck

data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord)

eval :: Expr -> Int
eval (Con n) = n
eval (Add x y) = eval x + eval y

-- Property: Add is commutative
prop_com_add :: Expr -> Expr -> Property
prop_com_add x y = eval (Add x y) === eval (Add y x)


-- Generator of Expr
expr :: Gen Expr
expr = oneof [ fmap Con arbitrary
             , do x <- expr
                  y <- expr
                  return $ Add x y
             ]

-- Take 1: may generate large expression
--instance Arbitrary Expr where
--  arbitrary = expr

-- Take 2: using sized generators
instance Arbitrary Expr where
   arbitrary = expr2

   shrink (Con n) = map Con $ shrink n
   shrink (Add e1 e2) =
     -- shrink to subterms
     [e1, e2] ++
     -- recursively shrink subterms
     [Add e1' e2' | (e1', e2') <- shrink (e1, e2)]


expr2 = sized exprN
exprN 0 = fmap Con arbitrary
exprN n = oneof [ fmap Con arbitrary
                , do x <- subexpr
                     y <- subexpr
                     return $ Add x y]
  where subexpr = exprN (n `div` 2)
