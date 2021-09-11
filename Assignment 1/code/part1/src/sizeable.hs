module Sizeable where

class Sizeable t where
  size :: t -> Int

instance Sizeable Int where
  size _ = 1

-- instance Sizeable [a] where
--   size l = length l

instance Sizeable a => Sizeable [a] where
  size [] = 0
  size xs = sum (map size xs) + length xs + 1 