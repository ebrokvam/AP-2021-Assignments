{-
Example code produced for AP lecture.

Using Tasty.HUnit to test the Morse module

Author: Ken Friis Larsen <kflarsen@diku.dk>
-}

import Test.Tasty
import Test.Tasty.HUnit
import Morse
import Test.Tasty.QuickCheck
import qualified MorseQuickCheck
import qualified MorseQuickCheck2
import qualified MorseQuickCheck3


testsuite =
  testGroup "Testing Morse encoding decoding"
  [ testGroup "Encoding"
    [ testCase "Encode SOFIA"
      (assertBool "SOFIA is not encoded as ...---..-....-"
       ("...---..-....-" == encode("SOFIA")))
    , testCase "Encode SOFIA (testing with @=?)"
      ("...---..-....-" @=? encode("SOFIA"))
    , testCase "Encode EUGENIA"
      ("...---..-....-" @=? encode("EUGENIA"))
    ]
  , testGroup "Decoding"
    [ testCase "Decode Eugenia"
      (assertBool "EUGENIA is not in the decodings of ...---..-....-"
       ("EUGENIA" `elem` decode "...---..-....-"))
    , testCase "Decode Sofia"
      (assertBool "SOFIA is not in the decodings of ...---..-....-"
       ("SOFIA" `elem` decode "...---..-....-"))
    ]
  , quickChecks
  ]

quickChecks =
  testGroup "QuickCheck tests"
  [ testProperty "With pre-condition"   MorseQuickCheck.prop_encode_decode
  , testProperty "ASCII-only generator" MorseQuickCheck2.prop_encode_decode
  , testProperty "Clever frequencies"   MorseQuickCheck3.prop_encode_decode
  ]

main = defaultMain testsuite
