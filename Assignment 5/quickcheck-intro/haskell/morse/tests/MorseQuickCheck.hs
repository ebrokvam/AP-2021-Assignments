{-
Example code produced "during" AP lecture.

Use QuickCheck to test the Morse module.

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module MorseQuickCheck where

import Test.QuickCheck
import qualified Morse
import qualified Data.Char as C

--prop_encode_decode s = (map C.toUpper s) `elem` Morse.decode (Morse.encode s)

-- Take 2 (remember to fix Morse.hs)
--prop_encode_decode s = (fmap C.toUpper s) `elem` Morse.decode (Morse.encode s)

-- Take 3
good :: String -> Bool
good = all $ \c -> C.isAscii c && C.isLetter c

prop_encode_decode s =
  good s ==> (fmap C.toUpper s) `elem` Morse.decode (Morse.encode s)
























-- version including classification of input
-- prop_encode_decode s =
--   classify (null s) "empty string" $
--     good s ==> (fmap C.toUpper s) `elem` Morse.decode (Morse.encode s)
