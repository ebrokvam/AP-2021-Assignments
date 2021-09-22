-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) interpreterTests

  -- [testCase "crash test" $
  --   execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
  --                                          (Const (IntVal 2))]),
  --            SExp (Var "hello")]
  --     @?= (["4"], Just (EBadVar "hello")),
  --  testCase "execute misc.ast from handout" $
  --    do pgm <- read <$> readFile "examples/misc.ast"
  --       out <- readFile "examples/misc.out"
  --       execute pgm @?= (lines out, Nothing)]

-- comp monad tests
interpreterTests :: TestTree
interpreterTests = testGroup "Tests for the boa interpreter"
    [testCase "return" $
        runComp (return ()) []
          @?= (Right (),[]),
    -- TODO: more here

    -- monad operation tests
    testCase "abort" $
      runComp (abort (EBadArg "this is a test")) []
        @?= (Left (EBadArg "this is a test"),[]),
    testCase "look an existing value" $
      runComp (look "x") [("x",(IntVal 5))]
        @?= (Right (IntVal 5),[]),
    testCase "look an non-existing value" $
      runComp (look "x") []
        @?= (Left (EBadVar "x"),[]),
    testCase "look after withBinding" $
      runComp (withBinding "x" (IntVal 5) (look "x")) []
        @?= (Right (IntVal 5),[]),
    testCase "output" $
      runComp (output "test") []
        @?= (Right (),["test"]),
    testCase "output two strings" $
      runComp (do
        _ <- output "test"
        _ <- output "test"
        return ()) []
        @?= (Right (),["test", "test"]),

    -- truthy tests
    testCase "truthy none" $
      truthy NoneVal
        @?= False,
    testCase "truthy false" $
      truthy FalseVal
        @?= False,
    testCase "truthy 0" $
      truthy (IntVal 0)
        @?= False,
    testCase "truthy empty string" $
      truthy (StringVal "")
        @?= False,
    testCase "truthy empty list" $
      truthy (ListVal [])
        @?= False,
    testCase "truthy true" $
      truthy (TrueVal)
        @?= True,
    testCase "truthy 1" $
      truthy (IntVal 1)
        @?= True,
    testCase "truthy string" $
      truthy (StringVal "yes")
        @?= True,
    testCase "truthy list" $
      truthy (ListVal [IntVal 1])
        @?= True,

    -- operate tests
    testCase "operate plus" $
      operate Plus (IntVal 1) (IntVal 1)
        @?= Right (IntVal 2),
    testCase "operate plus wrong value type" $
      operate Plus (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate minus" $
      operate Minus (IntVal 1) (IntVal 1)
        @?= Right (IntVal 0),
    testCase "operate minus wrong value type" $
      operate Minus (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate times" $
      operate Times (IntVal 3) (IntVal 1)
        @?= Right (IntVal 3),
    testCase "operate times wrong value type" $
      operate Times (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate div" $
      operate Div (IntVal 6) (IntVal 2)
        @?= Right (IntVal 3),
    testCase "operate div zero" $
      operate Div (IntVal 1) (IntVal 0)
        @?= Left "attempted division by zero",
    testCase "operate div wrong value type" $
      operate Div (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate mod" $
      operate Mod (IntVal 7) (IntVal 2)
        @?= Right (IntVal 1),
    testCase "operate mod zero" $
      operate Mod (IntVal 1) (IntVal 0)
        @?= Left "attempted division by zero",
    testCase "operate mod wrong value type" $
      operate Mod (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate eq true" $
      operate Eq (IntVal 6) (IntVal 6)
        @?= Right TrueVal,
    testCase "operate eq false" $
      operate Eq (TrueVal) (FalseVal)
        @?= Right FalseVal,
    testCase "operate less true" $
      operate Less (IntVal 6) (IntVal 8)
        @?= Right TrueVal,
    testCase "operate less false" $
      operate Less (IntVal 6) (IntVal 5)
        @?= Right FalseVal,
    testCase "operate less wrong value type" $
      operate Less (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate greater true" $
      operate Less (IntVal 6) (IntVal 5)
        @?= Right TrueVal,
    testCase "operate greater false" $
      operate Less (IntVal 6) (IntVal 8)
        @?= Right FalseVal,
    testCase "operate greater wrong value type" $
      operate Greater (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    testCase "operate in true" $
      operate In (IntVal 5) (ListVal [(IntVal 5)])
        @?= Right TrueVal,
    testCase "operate in false" $
      operate In (IntVal 5) (ListVal [(IntVal 8)])
        @?= Right FalseVal,
    testCase "operate in empty list" $
      operate In (IntVal 5) (ListVal [])
        @?= Right FalseVal,
    testCase "operate in wrong value type" $
      operate In (IntVal 5) (NoneVal)
        @?= Left "invalid value type for operation",
    
    -- range tests
    testCase "range, 3 args, n3 > 0" $
      runComp (apply "range" [IntVal 1,IntVal 5,IntVal 2]) []
        @?= (Right (ListVal [IntVal 1,IntVal 3]),[]),     
    testCase "range, 3 args, n3 > 0, n1 > n2" $
      runComp (apply "range" [IntVal 2,IntVal 1,IntVal 2]) []
        @?= (Right (ListVal []),[]),
    testCase "range, 3 args, n3 < 0" $
      runComp (apply "range" [IntVal 5,IntVal 1,IntVal (-2)]) []
        @?= (Right (ListVal [IntVal 3,IntVal 1]),[]),     
    testCase "range, 3 args, n3 < 0, n1 < n2" $
      runComp (apply "range" [IntVal 1,IntVal 2,IntVal (-2)]) []
        @?= (Right (ListVal []),[]),
    testCase "range, 3 args, n3 == 0" $
      runComp (apply "range" [IntVal 1,IntVal 5,IntVal 0]) []
        @?= (Left (EBadArg "step size cannot be zero in range function"),[]),
    testCase "range, 3 args, incorrect value type" $
      runComp (apply "range" [IntVal 1,TrueVal,IntVal 1]) []
        @?= (Left (EBadArg "incorrect list size or value types for range function"),[]),
    testCase "range, 2 args, n1 < n2" $
      runComp (apply "range" [IntVal 1,IntVal 3]) []
        @?= (Right (ListVal [IntVal 1,IntVal 2]),[]),     
    testCase "range, 2 args, n1 > n2" $
      runComp (apply "range" [IntVal 3,IntVal 1]) []
        @?= (Right (ListVal []),[]),     
    testCase "range, 2 args, incorrect value type" $
      runComp (apply "range" [IntVal 1,TrueVal]) []
        @?= (Left (EBadArg "incorrect list size or value types for range function"),[]),
    testCase "range, 1 arg" $
      runComp (apply "range" [IntVal 3]) []
        @?= (Right (ListVal [IntVal 1,IntVal 2]),[]),     
    testCase "range, 1 arg, incorrect value type" $
      runComp (apply "range" [TrueVal]) []
        @?= (Left (EBadArg "incorrect list size or value types for range function"),[]),
    
    -- print tests
    testCase "print simple values" $
      runComp (apply "print" [NoneVal, TrueVal, FalseVal, IntVal 0]) []
        @?= (Right NoneVal,["None True False 0"]),
    testCase "print two strings" $
      runComp (apply "print" [StringVal "string 1", StringVal "string 2"]) []
        @?= (Right NoneVal,["string 1 string 2"]),
    testCase "print listval" $
      runComp (apply "print" [ListVal [StringVal "string", IntVal 0]]) []
        @?= (Right NoneVal,["string 0"]),
    testCase "print nested listval" $
      runComp (apply "print" [ListVal [ListVal [IntVal 1], ListVal [IntVal 2]]]) []
        @?= (Right NoneVal,["[[1], [2]]"]),
    testCase "print empty list" $
      runComp (apply "print" []) []
        @?= (Right NoneVal,[""]),
    testCase "print complex" $
      runComp (apply "print" [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) []
        @?= (Right NoneVal,["42 foo [True, []] -1"]),
      
    -- eval tests
    testCase "eval with const" $
      runComp (eval (Const (IntVal 5))) []
        @?= (Right (IntVal 5),[]),
    testCase "eval with var" $
      runComp (eval (Var "x")) [("x",(IntVal 5))]
        @?= (Right (IntVal 5),[]),
    testCase "eval with oper" $ -- operate helper function already extensively tested
      runComp (eval (Oper Plus (Const (IntVal 1))(Const (IntVal 1)))) []
        @?= (Right (IntVal 2),[]),
    testCase "eval with oper runerror" $ 
      runComp (eval (Oper Div (Const (IntVal 1))(Var "x"))) [("X", NoneVal)]
        @?= (Left (EBadVar "x"),[]),
    testCase "eval with oper propagated runerror" $ 
      runComp (eval (Oper Div (Const (IntVal 1))(Var "x"))) []
        @?= (Left (EBadVar "x"),[]),
    testCase "eval with not (false)" $ -- truthy helper function already extensively tested
      runComp (eval (Not (Const (IntVal 0)))) []
        @?= (Right TrueVal,[]),
    testCase "eval with not (true)" $
      runComp (eval (Not (Const (IntVal 1)))) []
        @?= (Right FalseVal,[]),
    testCase "eval with call range" $ -- range function already extensively tested
      runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 5), Const (IntVal 2)])) []
        @?= (Right (ListVal [IntVal 1,IntVal 3]),[]),     
    testCase "eval with call range error" $
      runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 5), Const NoneVal])) []
        @?= (Left (EBadArg "incorrect list size or value types for range function"),[]),
    testCase "eval with call print" $ -- print function already extensively tests
      runComp (eval (Call "print" [Const (IntVal 42), Const (StringVal "foo"), Const (ListVal [TrueVal, ListVal []]), Const (IntVal (-1))])) []
        @?= (Right NoneVal,["42 foo [True, []] -1"]),   
    testCase "eval with call bad function" $
      runComp (eval (Call "foo" [Const (IntVal 0)])) []
        @?= (Left (EBadFun "foo"),[]),
    testCase "eval with list empty" $ 
      runComp (eval (List [])) []
        @?= (Right (ListVal []),[]),
    testCase "eval with list runerror" $ 
      runComp (eval (List [Var "x"])) []
        @?= (Left (EBadVar "x"),[]),
    testCase "eval with list runerror left to right" $ 
      runComp (eval (List [Var "x",Var "y"])) []
        @?= (Left (EBadVar "x"),[]),
    --TODO: compr

    -- exec tests
    testCase "exec sdef" $ 
      runComp (exec [SDef "x" (Const (IntVal 1))]) []
        @?= (Right (),[]),
    testCase "exec sexp" $ 
      runComp (exec [SExp (Const (IntVal 1))]) []
        @?= (Right (),[]),
    testCase "exec output" $
      runComp (exec [SExp (Call "print" [Const (StringVal "Hello")])]) []
        @?= (Right (),["Hello"]),
    testCase "exec sdef + sexp + output" $ 
      runComp (exec [SDef "x" (Const (StringVal "Hello")), SExp (Call "print" [Var "x"])]) []
        @?= (Right (),[]),
    testCase "exec runerror" $ 
      runComp (exec [SExp (Var "x")]) []
        @?= (Left (EBadVar "x"),[]),
    --TODO: execute tests
    testCase "execute misc.ast from handout" $
      do 
        pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),
    testCase "execute crash.ast from handout" $
      do 
        pgm <- read <$> readFile "examples/crash.ast"
        out <- readFile "examples/crash.out"
        execute pgm @?= (lines out, Nothing)]