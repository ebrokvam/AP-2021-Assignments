module Intro where

import Test.QuickCheck

prop_plusmax :: Int -> Int -> Int -> Property
prop_plusmax k x y = k + (max x y) === max (k + x) (k + y)

prop_int_plus_associative :: Int -> Int -> Int -> Property
prop_int_plus_associative x y z = x + (y + z) === (x + y) + z

prop_reverse :: [Int] -> Property
prop_reverse xs = reverse(reverse xs) === xs

prop_minusmax :: Int -> Int -> Int -> Property
prop_minusmax k x y = k - (max x y) === max (k - x) (k - y)

prop_double_plus_associative :: Double -> Double -> Double -> Property
prop_double_plus_associative x y z = x + (y + z) === (x + y) + z
