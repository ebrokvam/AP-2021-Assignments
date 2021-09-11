module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)

moves :: [Direction] -> Pos -> Pos
moves [] y = y
moves (x:xs) y = moves xs (move x y)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ y) x = add y (Succ x)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ y) x = add x (mult y x)

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = nat2int x+1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y a b)
  | x > y = Node y (insert x a) b
  | x < y = Node y a (insert x b)
  | otherwise = Node y a b

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Show) -- Just added this for testing purposes, hope that's okay

-- must have type class Ord
pinsert :: Ord a => a -> PTree a -> PTree a 
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode y a b)
  | x > y = PNode y (pinsert x a) b
  | x < y = PNode y a (pinsert x b)
  | otherwise = PNode y a b