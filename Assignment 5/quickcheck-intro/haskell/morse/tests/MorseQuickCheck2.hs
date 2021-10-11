{-
Example code produced "during" AP lecture.

Use QuickCheck to test the Morse module.  Second attempt, make a
generator for strings the only contains ASCII letter.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module MorseQuickCheck2 where

import Test.QuickCheck
import qualified Data.Char as C
import qualified Morse

upper = map C.toUpper

prop_encode_decode (LO s) = (upper s) `elem` Morse.decode (Morse.encode s)

newtype LettersOnly = LO String
                    deriving (Eq, Show)

asciiLetter = elements (['a'..'z'] ++ ['A'..'Z'])

--instance Arbitrary LettersOnly where
--   arbitrary = fmap LO $ listOf asciiLetter

-- Take 2
instance Arbitrary LettersOnly where
  arbitrary = fmap LO $ do
    n <- choose (0, 4)
    vectorOf n asciiLetter
