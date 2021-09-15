-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [  
  showExpCst,
  showExpAdd,
  showExpSub,
  showExpMul,
  showExpDiv,
  showExpPow,
  showExpNestedParenthesis1,
  showExpNestedParenthesis2,

  evalSimpleCst,
  evalSimpleAdd,
  evalSimpleSub,
  evalSimpleMul,
  evalSimpleDiv,
  evalSimpleDivFloor1,
  evalSimpleDivFloor2,
  evalSimplePow,
  evalSimplePowZero,

  evalFullCst,
  evalFullAdd,
  evalFullSub,
  evalFullMul,
  evalFullDiv,
  evalFullDivFloor1,
  evalFullDivFloor2,
  evalFullPow,
  evalFullPowZero,
  evalFullIf1,
  evalFullIf2,
  evalFullIfNoRuntimeError,
  evalFullVarInitEnv,
  evalFullVarExistingEnv,
  evalFullLet,
  evalFullLetNested,
  evalFullLetUnusedBind,
  evalFullLetLazyEval,
  evalFullSum,

  -- extendEnv is tested in evalFullVar and evalErrVar tests

  evalErrCst,
  evalErrAdd,
  evalErrSub,
  evalErrMul,
  evalErrDiv,
  evalErrDivFloor1,
  evalErrDivFloor2,
  evalErrDivZero,
  evalErrPow,
  evalErrPowNegative,
  evalErrPowZero,
  evalErrIf1,
  evalErrIf2,
  evalErrIfNoRuntimeError,
  evalErrVarInitEnv,
  evalErrVarExistingEnv,
  evalErrVarBadVar,
  evalErrLet,
  evalErrLetNested,
  evalErrLetUnusedBind,
  evalErrLetAttemptAccessUnbound,
  evalErrLetEagerEval,
  evalErrSum,
  evalErrSumToLessThanFrom,

  -- optional parts

  showCompactCst,
  showCompactAdd,
  showCompactSub,
  showCompactMul,
  showCompactDiv,
  showCompactPow,
  showCompactNestedParenthesis,
  showCompactComplex1,
  showCompactComplex2,
  showCompactComplex3,

  evalEagerCst,
  evalEagerAdd,
  evalEagerSub,
  evalEagerMul,
  evalEagerDiv,
  evalEagerDivFloor1,
  evalEagerDivFloor2,
  evalEagerDivZero,
  evalEagerPow,
  evalEagerPowNegative,
  evalEagerPowZero,
  evalEagerIf1,
  evalEagerIf2,
  evalEagerIfNoRuntimeError,
  evalEagerVarInitEnv,
  evalEagerVarExistingEnv,
  evalEagerVarBadVar,
  evalEagerLet,
  evalEagerLetNested,
  evalEagerLetUnusedBind,
  evalEagerLetAttemptAccessUnbound,
  evalEagerLetEagerEval,
  evalEagerSum,
  evalEagerSumToLessThanFrom,

  evalLazyCst,
  evalLazyAdd,
  evalLazySub,
  evalLazyMul,
  evalLazyDiv,
  evalLazyDivFloor1,
  evalLazyDivFloor2,
  evalLazyDivZero,
  evalLazyPow,
  evalLazyPowNegative,
  evalLazyPowZero,
  evalLazyIf1,
  evalLazyIf2,
  evalLazyIfNoRuntimeError,
  evalLazyVarInitEnv,
  evalLazyVarExistingEnv,
  evalLazyVarBadVar,
  evalLazyLet,
  evalLazyLetNested,
  evalLazyLetUnusedBind,
  evalLazyLetAttemptAccessUnbound,
  evalLazyLetLazyEval,
  evalLazySum,
  evalLazySumToLessThanFrom

  ] where
  showExpCst = ("showExpCst", showExp (Cst 2) == "2")
  showExpAdd = ("showExpAdd", showExp (Add (Cst 2) (Cst 2)) == "(2+2)")
  showExpSub = ("showExpSub", showExp (Sub (Cst 2) (Cst 2)) == "(2-2)")
  showExpMul = ("showExpMul", showExp (Mul (Cst 2) (Cst 2)) == "(2*2)")
  showExpDiv = ("showExpDiv", showExp (Div (Cst 2) (Cst 2)) == "(2`div`2)")
  showExpPow = ("showExpPow", showExp (Pow (Cst 2) (Cst 2)) == "(2^2)")
  showExpNestedParenthesis1 = ("showExpNestedParenthesis1", showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) == "((2*3)+4)")
  showExpNestedParenthesis2 = ("showExpNestedParenthesis2", showExp (Add (Div (Cst 15) (Cst 3)) (Cst 9)) == "((15`div`3)+9)")
  -- Should we upgarde our test suite, we would also test errors for:
  -- showExpIf
  -- showExpVar
  -- showExpLet
  -- showExpSum

  evalSimpleCst = ("evalSimpleCst", evalSimple (Cst 2) == 2)
  evalSimpleAdd = ("evalSimpleAdd", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  evalSimpleSub = ("evalSimpleSub", evalSimple (Sub (Cst 2) (Cst 2)) == 0)
  evalSimpleMul = ("evalSimpleMul", evalSimple (Mul (Cst 2) (Cst 2)) == 4)
  evalSimpleDiv = ("evalSimpleDiv", evalSimple (Div (Cst 2) (Cst 2)) == 1)
  evalSimpleDivFloor1 = ("evalSimpleDivFloor1", evalSimple (Div (Cst 7) (Cst 2)) == 3)
  evalSimpleDivFloor2 = ("evalSimpleDivFloor2", evalSimple (Div (Cst (-7)) (Cst 2)) == (-4))
  evalSimplePow = ("evalSimplePow", evalSimple (Pow (Cst 2) (Cst 2)) == 4)
  evalSimplePowZero = ("evalSimplePowZero", evalSimple (Pow (Cst 2) (Cst 0)) == 1)
  -- Should we upgarde our test suite, we would also test errors for:
  -- evalSimpleDivZero
  -- evalSimplePowNegative
  -- evalSimpleIf
  -- evalSimpleVar
  -- evalSimpleLet
  -- evalSimpleSum

  evalFullCst = ("evalFullCst", evalFull (Cst 2) initEnv == 2)
  evalFullAdd = ("evalFullAdd", evalFull (Add (Cst 2) (Cst 2)) initEnv == 4)
  evalFullSub = ("evalFullSub", evalFull (Sub (Cst 2) (Cst 2)) initEnv == 0)
  evalFullMul = ("evalFullMul", evalFull (Mul (Cst 2) (Cst 2)) initEnv == 4)
  evalFullDiv = ("evalFullDiv", evalFull (Div (Cst 2) (Cst 2)) initEnv == 1)
  evalFullDivFloor1 = ("evalFullDivFloor1", evalFull (Div (Cst 7) (Cst 2)) initEnv == 3)
  evalFullDivFloor2 = ("evalFullDivFloor2", evalFull (Div (Cst (-7)) (Cst 2)) initEnv == (-4))
  evalFullPow = ("evalFullPow", evalFull (Pow (Cst 2) (Cst 2)) initEnv == 4)
  evalFullPowZero = ("evalFullPowZero", evalFull (Pow (Cst 2) (Cst 0)) initEnv == 1)
  evalFullIf1 = ("evalFullIf1", evalFull (If (Cst 1) (Cst 1) (Cst 2)) initEnv == 1)
  evalFullIf2 = ("evalFullIf2", evalFull (If (Cst 0) (Cst 1) (Cst 2)) initEnv == 2)
  evalFullIfNoRuntimeError = ("evalFullIfNoRuntimeError", evalFull (If (Cst 1) (Cst 1) (Div (Cst 1) (Cst 0))) initEnv == 1)
  evalFullVarInitEnv = ("evalFullVarInitEnv", evalFull (Var "v") (extendEnv "v" 1 initEnv) == 1)
  evalFullVarExistingEnv = ("evalFullVarExistingEnv", evalFull (Var "b") (extendEnv "v" 1 (extendEnv "b" 4 initEnv)) == 4)
  evalFullLet = ("evalFullLet", evalFull (Let "v" (Cst 1) (Add (Var "v") (Cst 5))) initEnv == 6)
  evalFullLetNested = ("evalFullLetNested", evalFull (Let "v" (Cst 1) (Let "v" (Cst 3) (Add (Var "v") (Cst 5)))) initEnv == 9)
  evalFullLetUnusedBind = ("evalFullLetUnusedBind", evalFull (Let "v" (Cst 1) (Cst 1)) initEnv == 1)
  evalFullLetLazyEval = ("evalFullLetLazyEval", evalFull (Let "v" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv == 5)
  evalFullSum = ("evalFullSum", evalFull (Sum "v" (Cst 1) (Cst 4) (Mul (Var "v") (Var "v"))) initEnv == 30)
  -- Should we upgarde our test suite, we would also test errors for:
  -- evalFullDivZero
  -- evalFullPowNegative
  -- evalFullVarBadVar
  -- evalFullAttemptAccessUnbound
  -- evalFullSumToLessThanFrom

  -- extendEnv is tested in evalFullVar and evalErrVar tests

  evalErrCst = ("evalErrCst", evalErr (Cst 2) initEnv == Right 2)
  evalErrAdd = ("evalErrAdd", evalErr (Add (Cst 2) (Cst 2)) initEnv == Right 4)
  evalErrSub = ("evalErrSub", evalErr (Sub (Cst 2) (Cst 2)) initEnv == Right 0)
  evalErrMul = ("evalErrMul", evalErr (Mul (Cst 2) (Cst 2)) initEnv== Right 4)
  evalErrDiv = ("evalErrDiv", evalErr (Div (Cst 2) (Cst 2)) initEnv== Right 1)
  evalErrDivFloor1 = ("evalErrDivFloor1", evalErr (Div (Cst 7) (Cst 2)) initEnv == Right 3)
  evalErrDivFloor2 = ("evalErrDivFloor2", evalErr (Div (Cst (-7)) (Cst 2)) initEnv == Right (-4))
  evalErrDivZero = ("evalErrDivZero", evalErr (Div (Cst 2) (Cst 0)) initEnv == Left EDivZero)
  evalErrPow = ("evalErrPow", evalErr (Pow (Cst 2) (Cst 2)) initEnv == Right 4)
  evalErrPowNegative = ("evalErrPowNegative", evalErr (Pow (Cst 2) (Cst (-2))) initEnv == Left ENegPower)
  evalErrPowZero = ("evalErrPowZero", evalErr (Pow (Cst 2) (Cst 0)) initEnv == Right 1)
  evalErrIf1 = ("evalErrIf1", evalErr (If (Cst 1) (Cst 1) (Cst 2)) initEnv == Right 1)
  evalErrIf2 = ("evalErrIf2", evalErr (If (Cst 0) (Cst 1) (Cst 2)) initEnv == Right 2)
  evalErrIfNoRuntimeError = ("evalErrIfNoRuntimeError", evalErr (If (Cst 1) (Cst 1) (Div (Cst 1) (Cst 0))) initEnv == Right 1)
  evalErrVarInitEnv = ("evalErrVarInitEnv", evalErr (Var "v") (extendEnv "v" 1 initEnv) == Right 1)
  evalErrVarExistingEnv = ("evalErrVarExistingEnv", evalErr (Var "b") (extendEnv "v" 1 (extendEnv "b" 4 initEnv)) == Right 4)
  evalErrVarBadVar = ("evalErrVarBadVar",  evalErr (Var "v") initEnv == Left (EBadVar "v"))
  evalErrLet = ("evalErrLet", evalErr (Let "v" (Cst 1) (Add (Var "v") (Cst 5))) initEnv == Right 6)
  evalErrLetNested = ("evalErrLetNested", evalErr (Let "v" (Cst 1) (Let "v" (Cst 3) (Add (Var "v") (Cst 5)))) initEnv == Right 9)
  evalErrLetUnusedBind = ("evalErrLetUnusedBind", evalErr (Let "v" (Cst 1) (Cst 1)) initEnv == Right 1)
  evalErrLetAttemptAccessUnbound = ("evalErrLetAttemptAccessUnbound", evalErr (Let "v" (Cst 1) (Add (Cst 1) (Var "x"))) initEnv == Left (EBadVar "x"))
  evalErrLetEagerEval = ("evalErrLetEagerEval", evalErr (Let "v" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv == Left EDivZero)
  evalErrSum = ("evalErrSum", evalErr (Sum "v" (Cst 1) (Cst 4) (Mul (Var "v") (Var "v"))) initEnv == Right 30)
  evalErrSumToLessThanFrom = ("evalErrSumToLessThanFrom", evalErr (Sum "v" (Cst 2) (Cst 1) (Mul (Var "v") (Var "v"))) initEnv == Right 0)

  -- optional parts

  showCompactCst = ("showCompactCst", showCompact (Cst 2) == "2")
  showCompactAdd = ("showCompactAdd", showCompact (Add (Cst 2) (Cst 2)) == "2+2")
  showCompactSub = ("showCompactSub", showCompact (Sub (Cst 2) (Cst 2)) == "2-2")
  showCompactMul = ("showCompactMul", showCompact (Mul (Cst 2) (Cst 2)) == "2*2")
  showCompactDiv = ("showCompactDiv", showCompact (Div (Cst 2) (Cst 2)) == "2`div`2")
  showCompactPow = ("showCompactPow", showCompact (Pow (Cst 2) (Cst 2)) == "2^2")
  showCompactNestedParenthesis = ("showCompactNestedParenthesis", showCompact (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) == "2*3+4")
  showCompactComplex1 = ("showCompactComplex1", showCompact (Add (Cst 2) (Mul (Cst 3) (Cst 4))) == "2+3*4")
  showCompactComplex2 = ("showCompactComplex2", showCompact (Add (Cst 2) (Add (Cst 3) (Cst 4))) == "2+(3+4)")
  showCompactComplex3 = ("showCompactComplex3", showCompact (Pow (Cst 2) (Pow (Cst 3) (Cst 4))) == "2^(3^4)")
  -- Should we upgarde our test suite, we would also test errors for:
  -- showCompactIf
  -- showCompactVar
  -- showCompactLet
  -- showCompactSum

  evalEagerCst = ("evalEagerCst", evalEager (Cst 2) initEnv == Right 2)
  evalEagerAdd = ("evalEagerAdd", evalEager (Add (Cst 2) (Cst 2)) initEnv == Right 4)
  evalEagerSub = ("evalEagerSub", evalEager (Sub (Cst 2) (Cst 2)) initEnv == Right 0)
  evalEagerMul = ("evalEagerMul", evalEager (Mul (Cst 2) (Cst 2)) initEnv== Right 4)
  evalEagerDiv = ("evalEagerDiv", evalEager (Div (Cst 2) (Cst 2)) initEnv== Right 1)
  evalEagerDivFloor1 = ("evalEagerDivFloor1", evalEager (Div (Cst 7) (Cst 2)) initEnv == Right 3)
  evalEagerDivFloor2 = ("evalEagerDivFloor2", evalEager (Div (Cst (-7)) (Cst 2)) initEnv == Right (-4))
  evalEagerDivZero = ("evalEagerDivZero", evalEager (Div (Cst 2) (Cst 0)) initEnv == Left EDivZero)
  evalEagerPow = ("evalEagerPow", evalEager (Pow (Cst 2) (Cst 2)) initEnv == Right 4)
  evalEagerPowNegative = ("evalEagerPowNegative", evalEager (Pow (Cst 2) (Cst (-2))) initEnv == Left ENegPower)
  evalEagerPowZero = ("evalEagerPowZero", evalEager (Pow (Cst 2) (Cst 0)) initEnv == Right 1)
  evalEagerIf1 = ("evalEagerIf1", evalEager (If (Cst 1) (Cst 1) (Cst 2)) initEnv == Right 1)
  evalEagerIf2 = ("evalEagerIf2", evalEager (If (Cst 0) (Cst 1) (Cst 2)) initEnv == Right 2)
  evalEagerIfNoRuntimeError = ("evalEagerIfNoRuntimeError", evalEager (If (Cst 1) (Cst 1) (Div (Cst 1) (Cst 0))) initEnv == Right 1)
  evalEagerVarInitEnv = ("evalEagerVarInitEnv", evalEager (Var "v") (extendEnv "v" 1 initEnv) == Right 1)
  evalEagerVarExistingEnv = ("evalEagerVarExistingEnv", evalEager (Var "b") (extendEnv "v" 1 (extendEnv "b" 4 initEnv)) == Right 4)
  evalEagerVarBadVar = ("evalEagerVarBadVar",  evalEager (Var "v") initEnv == Left (EBadVar "v"))
  evalEagerLet = ("evalEagerLet", evalEager (Let "v" (Cst 1) (Add (Var "v") (Cst 5))) initEnv == Right 6)
  evalEagerLetNested = ("evalEagerLetNested", evalEager (Let "v" (Cst 1) (Let "v" (Cst 3) (Add (Var "v") (Cst 5)))) initEnv == Right 9)
  evalEagerLetUnusedBind = ("evalEagerLetUnusedBind", evalEager (Let "v" (Cst 1) (Cst 1)) initEnv == Right 1)
  evalEagerLetAttemptAccessUnbound = ("evalEagerLetAttemptAccessUnbound", evalEager (Let "v" (Cst 1) (Add (Cst 1) (Var "x"))) initEnv == Left (EBadVar "x"))
  evalEagerLetEagerEval = ("evalEagerLetEagerEval", evalEager (Let "v" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv == Left EDivZero)
  evalEagerSum = ("evalEagerSum", evalEager (Sum "v" (Cst 1) (Cst 4) (Mul (Var "v") (Var "v"))) initEnv == Right 30)
  evalEagerSumToLessThanFrom = ("evalEagerSumToLessThanFrom", evalEager (Sum "v" (Cst 2) (Cst 1) (Mul (Var "v") (Var "v"))) initEnv == Right 0)

  evalLazyCst = ("evalLazyCst", evalLazy (Cst 2) initEnv == Right 2)
  evalLazyAdd = ("evalLazyAdd", evalLazy (Add (Cst 2) (Cst 2)) initEnv == Right 4)
  evalLazySub = ("evalLazySub", evalLazy (Sub (Cst 2) (Cst 2)) initEnv == Right 0)
  evalLazyMul = ("evalLazyMul", evalLazy (Mul (Cst 2) (Cst 2)) initEnv== Right 4)
  evalLazyDiv = ("evalLazyDiv", evalLazy (Div (Cst 2) (Cst 2)) initEnv== Right 1)
  evalLazyDivFloor1 = ("evalLazyDivFloor1", evalLazy (Div (Cst 7) (Cst 2)) initEnv == Right 3)
  evalLazyDivFloor2 = ("evalLazyDivFloor2", evalLazy (Div (Cst (-7)) (Cst 2)) initEnv == Right (-4))
  evalLazyDivZero = ("evalLazyDivZero", evalLazy (Div (Cst 2) (Cst 0)) initEnv == Left EDivZero)
  evalLazyPow = ("evalLazyPow", evalLazy (Pow (Cst 2) (Cst 2)) initEnv == Right 4)
  evalLazyPowNegative = ("evalLazyPowNegative", evalLazy (Pow (Cst 2) (Cst (-2))) initEnv == Left ENegPower)
  evalLazyPowZero = ("evalLazyPowZero", evalLazy (Pow (Cst 2) (Cst 0)) initEnv == Right 1)
  evalLazyIf1 = ("evalLazyIf1", evalLazy (If (Cst 1) (Cst 1) (Cst 2)) initEnv == Right 1)
  evalLazyIf2 = ("evalLazyIf2", evalLazy (If (Cst 0) (Cst 1) (Cst 2)) initEnv == Right 2)
  evalLazyIfNoRuntimeError = ("evalLazyIfNoRuntimeError", evalLazy (If (Cst 1) (Cst 1) (Div (Cst 1) (Cst 0))) initEnv == Right 1)
  evalLazyVarInitEnv = ("evalLazyVarInitEnv", evalLazy (Var "v") (extendEnv "v" 1 initEnv) == Right 1)
  evalLazyVarExistingEnv = ("evalLazyVarExistingEnv", evalLazy (Var "b") (extendEnv "v" 1 (extendEnv "b" 4 initEnv)) == Right 4)
  evalLazyVarBadVar = ("evalLazyVarBadVar",  evalLazy (Var "v") initEnv == Left (EBadVar "v"))
  evalLazyLet = ("evalLazyLet", evalLazy (Let "v" (Cst 1) (Add (Var "v") (Cst 5))) initEnv == Right 6)
  evalLazyLetNested = ("evalLazyLetNested", evalLazy (Let "v" (Cst 1) (Let "v" (Cst 3) (Add (Var "v") (Cst 5)))) initEnv == Right 9)
  evalLazyLetUnusedBind = ("evalLazyLetUnusedBind", evalLazy (Let "v" (Cst 1) (Cst 1)) initEnv == Right 1)
  evalLazyLetAttemptAccessUnbound = ("evalLazyLetAttemptAccessUnbound", evalLazy (Let "v" (Cst 1) (Add (Cst 1) (Var "x"))) initEnv == Left (EBadVar "x"))
  evalLazyLetLazyEval = ("evalLazyLetLazyEval", evalLazy (Let "v" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv == Right 5)
  evalLazySum = ("evalLazySum", evalLazy (Sum "v" (Cst 1) (Cst 4) (Mul (Var "v") (Var "v"))) initEnv == Right 30)
  evalLazySumToLessThanFrom = ("evalLazySumToLessThanFrom", evalLazy (Sum "v" (Cst 2) (Cst 1) (Mul (Var "v") (Var "v"))) initEnv == Right 0)
main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
