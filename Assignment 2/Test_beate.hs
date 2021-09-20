-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),

  testCase "execute misc.ast from handout" $
    do pgm <- read <$> readFile "examples/misc.ast"
       out <- readFile "examples/misc.out"
       execute pgm @?= (lines out, Nothing),


  ------------------------
  -- operate Plus v1 v2 --
  ------------------------
  testCase "oper plus - pass" $
    execute [SExp (Call "print"
                        [Oper Plus (Const (IntVal 12))
                                   (Const (IntVal 8))])]
      @?=(["20"], Nothing),

  testCase "oper plus - fail" $
    execute [SExp (Call "print"
                        [Oper Plus (Const (IntVal 12))
                                   (Const (StringVal "null"))])]
      @?=([], Just (EBadArg "Invalid argument")),


  -------------------------
  -- operate Minus v1 v2 --
  -------------------------
  testCase "oper minus - pass" $
    execute [SExp (Call "print"
                        [Oper Minus (Const (IntVal 3))
                                    (Const (IntVal 5))])]
      @?=(["-2"], Nothing),

  testCase "oper minus - fail" $
    execute [SExp (Call "print"
                        [Oper Minus (Const (StringVal "ole"))
                                    (Const (IntVal 5))])]
      @?=([], Just (EBadArg "Invalid argument")),


  -------------------------
  -- operate Times v1 v2 --
  -------------------------
  testCase "oper times - pass" $
    execute [SExp (Call "print"
                        [Oper Times (Const (IntVal 3))
                                    (Const (IntVal 5))])]
      @?=(["15"], Nothing),

  testCase "oper times - fail" $
    execute [SExp (Call "print"
                        [Oper Times (Const (IntVal 3))
                                    (Const (StringVal "konge"))])]
      @?=([], Just (EBadArg "Invalid argument")),

  -----------------------
  -- operate Div v1 v2 --
  -----------------------
  testCase "oper div - pass" $
    execute [SExp (Call "print"
                        [Oper Div (Const (IntVal 20))
                                  (Const (IntVal 5))])]
      @?= (["4"], Nothing),

  testCase "oper div - fail" $
    execute [SExp (Call "print" 
                        [Oper Div (Const (IntVal 1)) 
                                  (Const (IntVal 0))])]
      @?=([], Just (EBadArg "Div by zero")),
  

  -----------------------
  -- operate Mod v1 v2 --
  -----------------------
  testCase "oper mod - pass" $
    execute [SExp (Call "print"
                        [Oper Mod (Const (IntVal 10))
                                  (Const (IntVal 3))])]
      @?= (["1"], Nothing),

  testCase "oper mod - fail" $
    execute [SExp (Call "print"
                        [Oper Mod (Const (IntVal 10))
                                  (Const (IntVal 0))])]
      @?=([], Just (EBadArg "Mod by zero")),
  

  ----------------------
  -- operate Eq v1 v2 --
  ----------------------
  testCase "oper eq two int - pass" $
    execute [SExp (Call "print" 
                        [Oper Eq (Const (IntVal 12))
                                 (Const (IntVal 12))])]
      @?=(["True"], Nothing),

  testCase "oper eq two string - pass" $
    execute [SExp (Call "print" 
                        [Oper Eq (Const (StringVal "marcipan"))
                                 (Const (StringVal "marcipan"))])]
      @?=(["True"], Nothing),   

  testCase "oper eq list of int - pass" $
    execute [SExp (Call "print" 
                        [Oper Eq (List 
                                 [Const (IntVal 1), 
                                  Const (IntVal 2)]) 
                                 (List 
                                  [Const (IntVal 1), 
                                  Const (IntVal 2)]) ])]
      @?=(["True"], Nothing),

  testCase "oper eq list of strings - pass" $
    execute [SExp (Call "print" 
                        [Oper Eq (List 
                                 [Const (StringVal "tom"), 
                                  Const (StringVal "jerry")]) 
                                 (List 
                                  [Const (StringVal "tom"), 
                                  Const (StringVal "jerry")]) ])]
      @?=(["True"], Nothing),

  testCase "oper eq two int - fail" $
    execute [SExp (Call "print" 
                        [Oper Eq (Const (IntVal 12))
                                 (Const (IntVal 13))])]
      @?=(["False"], Nothing),  

  testCase "oper eq two string - fail" $
    execute [SExp (Call "print" 
                        [Oper Eq (Const (StringVal "xmas"))
                                 (Const (StringVal "påske"))])]
      @?=(["False"], Nothing),  

  testCase "oper eq int,string - fail" $
    execute [SExp (Call "print" 
                        [Oper Eq (Const (IntVal 1))
                                 (Const (StringVal "1"))])]
      @?=(["False"], Nothing),

  testCase "oper eq list of int - fail " $
    execute [SExp (Call "print" 
                        [Oper Eq (List 
                                 [Const (IntVal 1), 
                                  Const (IntVal 2)]) 
                                 (List 
                                  [Const (IntVal 3), 
                                  Const (IntVal 4)]) ])]
      @?=(["False"], Nothing),

  testCase "oper eq list of strings - fail" $
    execute [SExp (Call "print" 
                        [Oper Eq (List 
                                 [Const (StringVal "tom"), 
                                  Const (StringVal "jerry")]) 
                                 (List 
                                  [Const (StringVal "pjuske"), 
                                  Const (StringVal "jack")]) ])]
      @?=(["False"], Nothing),

  --------------
  ---- misc ----
  --------------
  testCase "assigning x = 4 - pass" $
    execute [SDef "x" (Const (IntVal 4)), 
             SExp (Call "print" [Var "x"])]
    @?=(["4"], Nothing),

  --------------
  ---- print ---
  --------------
  testCase "print 'Hello World!' - pass" $
    execute [SExp (Call "print" 
                  [Const (StringVal "hello world!")])]
    @?=(["hello world!"], Nothing),
  
  testCase "print [[1,2],None,[]] - pass" $
     execute [SExp (Call "print" 
                   [List [List 
                         [Const (IntVal 1), 
                          Const (IntVal 2)], 
                          Const NoneVal, 
                          List [] ]])]
    @?=(["[[1, 2], None, []]"], Nothing),

  testCase "print 'foo' 42 True - pass" $
     execute [SExp (Call "print" [Const (StringVal "foo"),
                                  Const (IntVal 42), 
                                  Const TrueVal])]
    @?=(["foo 42 True"], Nothing),  

  -----------------------
  -- test list in list --
  -----------------------
  testCase "test list in list - pass" $
    execute [SExp (Call "print" 
                        [List [Const (IntVal 1), 
                               Const (IntVal 2), 
                               List [Const (StringVal "hest")]]])]
  @?=(["[1, 2, [hest]]"], Nothing),

  ----------------
  -- test range --
  ----------------
  testCase "range(4) - pass" $
    execute [SExp (Call "print" 
                  [(Call "range" [Const (IntVal 4)])])]
    @?=(["[0, 1, 2, 3]"], Nothing),

   testCase "range(1,9) - pass" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 1), 
                           Const (IntVal 9)])])]
    @?=(["[1, 2, 3, 4, 5, 6, 7, 8]"], Nothing),

  testCase "range(10,1,-3) - pass" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 10), 
                           Const (IntVal 1), 
                           Const (IntVal (-3)) ])])]
    @?=(["[10, 7, 4]"], Nothing),

  testCase "range(2,8,2) - pass" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 2), 
                           Const (IntVal 8), 
                           Const (IntVal 2)])])]
    @?=(["[2, 4, 6]"], Nothing),

  testCase "range() - fail" $
    execute [SExp (Call "print" 
                  [(Call "range" [])])]
    @?=([], Just (EBadArg "bad arguments")),

  testCase "range(3,5,None) - fail" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 3), 
                           Const (IntVal 5), 
                           Const NoneVal])])]
    @?=([], Just (EBadArg "bad arguments")),
  
  testCase "range(3,5,7,8) - fail" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 3), 
                           Const (IntVal 5), 
                           Const (IntVal 7), 
                           Const (IntVal 5)])])]
    @?=([], Just (EBadArg "bad arguments")),

  testCase "range(1,2,3,None) - fail" $
    execute [SExp (Call "print" 
                  [(Call "range" 
                          [Const (IntVal 1), 
                           Const (IntVal 2), 
                           Const (IntVal 3), 
                           Const NoneVal])])]
    @?=([], Just (EBadArg "bad arguments")),

  -----------------------------
  -- test list comprehension --
  -----------------------------
  testCase "[(x+1) for x in [2,5]] - pass" $
    execute [SExp (Call "print" 
                        [Compr (Oper Plus (Var "x")
                                          (Const (IntVal 1))) 
                                                [CCFor "x" (List [(Const (IntVal 2)),
                                                                  (Const (IntVal 5))])]])]
    @?=(["[3, 6]"], Nothing),

  testCase "[x if (not x)] - fail" $
     execute [SExp (Call "print" 
                   [Compr (Var "x") 
                            [CCIf (Not (Var "x"))]])]
     @?=([],Just (EBadVar "x")),

  testCase "[x for x in range(5) - pass" $
    execute [SExp (Call "print" 
                        [Compr (Var "x") 
                              [CCFor "x" (Call "range" [Const (IntVal 5)])]])]
    @?=(["[0, 1, 2, 3, 4]"], Nothing),

  testCase "[print(x) for x in range(5) - pass" $
    execute [SExp (Call "print" 
                        [Compr (Call "print" [Var "x"])
                              [CCFor "x" (Call "range" [Const (IntVal 5)])]])]
    @?=(["0", "1", "2", "3", "4","[None, None, None, None, None]"], Nothing),
 
  testCase "[x for x in range(9) if (x%2)] - pass" $
    execute [SExp (Call "print" 
                        [Compr (Var "x") 
                              [CCFor "x" (Call "range" [Const (IntVal 9)]),
                              CCIf (Oper Mod (Var "x")
                                             (Const (IntVal 2)))]])]
    @?=(["[1, 3, 5, 7]"], Nothing),
   
  testCase "[print(x) for x in range(9) if (x%2)] - pass" $
    execute [SExp (Call "print" 
                        [Compr (Call "print" [Var "x"])
                              [CCFor "x" (Call "range" [Const (IntVal 9)]),
                              CCIf (Oper Mod (Var "x")
                                             (Const (IntVal 2)))]])]
    @?=(["1", "3", "5", "7","[None, None, None, None]"], Nothing)
  ]