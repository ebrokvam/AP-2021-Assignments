-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test_show0, 
         test_show1, 
         test_evalSimp0, 
         test_evalSimp1,
         test_evalSimp2,
         test_evalSimp3,
         test_evalSimp4,
         test_evalSimp5] where
  test_show0 = ("test_showExp-0", showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) == "((2)*((3)+(4)))")
  test_show1 = ("test_showExp-1", showExp (Add (Div (Cst 15) (Cst 3)) (Cst 9)) == "(((15)`div`(3))+(9))")

  -- simple evaluation test --
  test_evalSimp0 = ("test02_evalSimple_Add", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test_evalSimp1 = ("test03_evalSimple_Sub", evalSimple (Sub (Cst 44) (Cst 2)) == 42)
  test_evalSimp2 = ("test04_evalSimple_Mul", evalSimple (Mul (Cst 12) (Cst 2)) == 24)
  test_evalSimp3 = ("test05_evalSimple_Div", evalSimple (Div (Cst 7) (Cst 2)) == 3)
  test_evalSimp4 = ("test06_evalSimple_Pow", evalSimple (Pow (Cst 3) (Cst 2)) == 9)
  test_evalSimp5 = ("test07_evalSimple_PowZero", evalSimple (Pow (Div (Cst 0) (Cst 0)) (Cst 0)) /= 1)


  --test03 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  --test04 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

  -- testing the optional implementations --
  --test_showComp0 = ("test_showComp-0", showCompact (Add (Cst 2) (Mul (Cst 3) (Cst 4))) == "2+3*4")
  --test_showComp1 = ("test_showComp-1", showCompact (Add (Cst 2) (Add (Cst 3) (Cst 4))) == "2+(3+4)")
  --test_showComp2 = ("test_showComp-2", showCompact (Pow (Cst 2) (Pow (Cst 3) (Cst 4))) == "2^3^4")

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
