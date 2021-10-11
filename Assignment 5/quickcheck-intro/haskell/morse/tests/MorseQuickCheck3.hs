{-
Example code produced "during" AP lecture.

Use QuickCheck to test the Morse module.

Third attempt, make a generator for strings that only contains ASCII
letters. Generate letters taking their length of their morse encoding
into account, letters with short encoding should occur more often.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

module MorseQuickCheck3 where

import Test.QuickCheck
import qualified Data.Char as C
import qualified Morse

upper = map C.toUpper

prop_encode_decode (LO s) = upper s `elem` Morse.decode (Morse.encode s)


weightedLetters = frequency [ (2^(max - length code), return c)
                            | (c, code) <- Morse.charMap]
  where max = 1 + (maximum $ map (length . snd) Morse.charMap)

newtype LettersOnly = LO String
                    deriving (Eq, Show)

instance Arbitrary LettersOnly where
  arbitrary = fmap LO $ do n <- choose (0, 7)
                           vectorOf n weightedLetters
