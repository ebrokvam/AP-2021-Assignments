-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Parser tests" [
  -- testCase "simple success" $
  --   parseString "2 + two" @?=
  --     Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  -- testCase "simple failure" $
  --   -- avoid "expecting" very specific parse-error messages
  --   case parseString "wow!" of
  --     Left e -> return ()  -- any message is OK
  --     Right p -> assertFailure $ "Unexpected parse: " ++ show p]
      
      -- space tests
      testCase "space at start" $
        parseString " x" @?=
          Right [SExp (Var "x")],
      testCase "tab at start" $
        parseString "\tx" @?=
          Right [SExp (Var "x")],
      testCase "newline at start" $
        parseString "\nx" @?=
          Right [SExp (Var "x")],

      -- numConst tests
      testCase "1" $
        parseString "1" @?=
          Right [SExp (Const (IntVal 1))],
      testCase "-1" $
        parseString "-1" @?=
          Right [SExp (Const (IntVal (-1)))],
      testCase "0 " $
        parseString " 0 " @?=
          Right [SExp (Const (IntVal (0)))],
      testCase "-0" $
        parseString "-0" @?=
          Right [SExp (Const (IntVal (-0)))],
      testCase "int overflow" $
        parseString "18446744073709551615" @?=
          Right [SExp (Const (IntVal (-1)))],
      testCase "00" $
        case parseString "00" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "007" $
        case parseString "007" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "- 1" $
        case parseString "- 1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "+1" $
        case parseString "+1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

      -- ident tests
      testCase "x" $
        parseString "x" @?=
          Right [SExp (Var "x")],
      testCase "x=1" $
        parseString "x=1" @?=
          Right [SDef "x" (Const (IntVal 1))],
      testCase "x = 1" $
        parseString "x = 1" @?=
          Right [SDef "x" (Const (IntVal 1))],
      testCase "var" $
        parseString "var" @?=
          Right [SExp (Var "var")],
      testCase "not1" $
        parseString "not1" @?=
          Right [SExp (Var "not1")],
      testCase "false1" $
        parseString "false1" @?=
          Right [SExp (Var "false1")],
      testCase "var_1" $
        parseString "var_1" @?=
          Right [SExp (Var "var_1")],
      testCase "_var1" $
        parseString "_var1" @?=
          Right [SExp (Var "_var1")],
      testCase "1_var" $
        case parseString "1_var" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      --reserved words in ident
      testCase "None=1" $
        case parseString "None=1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "True = 1" $
        case parseString "True = 1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase " False =   1" $
        case parseString " False =   1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "for" $
        case parseString "for" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "if()" $
        case parseString "if()" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "in=1" $
        case parseString "in=1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "not=1" $
        case parseString "not=1" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

      -- stringConst tests
      testCase "'a'" $
        parseString "\'a\'" @?=
          Right [SExp (Const (StringVal "a"))],
      testCase "'hello world'" $
        parseString "\'hello world\'" @?=
          Right [SExp (Const (StringVal "hello world"))],
      testCase "'!\"#$%&()*+,-./:;<=>?[]^_@`}{|~'" $ --TODO: ascii delete is printable?
        parseString "'!\"#$%&()*+,-./:;<=>?[]^_@`}{|~'" @?=
          Right [SExp (Const (StringVal"!\"#$%&()*+,-./:;<=>?[]^_@`}{|~"))],    
      testCase "'x\\y\\z'" $
        parseString "'x\\\\y\\\\z'" @?=
          Right [SExp (Const (StringVal "x\\y\\z"))],
      testCase "'x\\y\'z'" $
        parseString "'x\\\\y\'z'" @?=
          Right [SExp (Const (StringVal "x\\y'z"))],
      testCase "'a\\nb'" $
        parseString "'a\\nb'" @?=
          Right [SExp (Const (StringVal "a\nb"))],
      testCase "'fo\\\\o\\b\\na\\\'r'" $
        parseString "'fo\\\\o\\b\\na\\\'r'" @?=
          Right [SExp (Const (StringVal "fo\\ob\na'r"))],
      testCase "'a\"b\\n'" $
        parseString "'a\"b\\n'" @?=
          Right [SExp (Const (StringVal "a\"b\\n"))],
      testCase "'\\n'" $
        parseString "'\n'" @?=
          Right [SExp (Const (StringVal "\n"))],
      testCase "'ë'" $
        parseString "'ë'" @?=
          Right [SExp (Const (StringVal "\235"))],
      testCase "'\\t'" $
        case parseString "'\t'" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "'\\DEL'" $
        case parseString "'\DEL'" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "'\\'" $
        case parseString "'\\'" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "'\''" $
        case parseString "'\''" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

      -- other expr tests
      testCase "NoneVal" $
        parseString "None" @?=
          Right [SExp (Const NoneVal)],
      testCase "TrueVal" $
        parseString "True " @?=
          Right [SExp (Const TrueVal)],
      testCase "FalseVal" $
        parseString "False" @?=
          Right [SExp (Const FalseVal)],
          -- oper tested comprehensively below
      testCase "not 1" $
        parseString "not 1" @?=
          Right [SExp (Not (Const (IntVal 1)))],
      testCase "not(1)" $
        parseString "not(1)" @?=
          Right [SExp (Not (Const (IntVal 1)))],
      testCase "not not 1" $
        parseString "not not 1" @?=
          Right [SExp (Not (Not (Const (IntVal 1))))],
      testCase "(1)" $
        parseString "(1)" @?=
          Right [SExp (Const (IntVal 1))],
      testCase "(((1)))" $
        parseString "(((1)))" @?=
          Right [SExp (Const (IntVal 1))],
      testCase "( 1 )" $
        parseString "( 1 )" @?=
          Right [SExp (Const (IntVal 1))],
      testCase "f()" $
        parseString "f()" @?=
          Right [SExp (Call "f" [])],
      testCase "f(x)" $
        parseString "f(x)" @?=
          Right [SExp (Call "f" [Var "x"])],
      testCase "f ( x < 4 ) " $
        parseString "f ( x < 4 ) " @?=
          Right [SExp (Call "f" [Oper Less (Var "x") (Const (IntVal 4))])],
      testCase "[]" $
        parseString "[]" @?=
          Right [SExp (List [])],
      testCase "[1]" $
        parseString "[1]" @?=
          Right [SExp (List [Const (IntVal 1)])],
      testCase "[1,2]" $
        parseString "[1,2]" @?=
          Right [SExp (List [Const (IntVal 1),Const (IntVal 2)])],
      testCase "[1, 2, 3]" $
        parseString "[1,2,3]" @?=
          Right [SExp (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])],
      testCase "[(1+2), True, 'yes']" $
        parseString "[(1+2), True, 'yes']" @?=
          Right [SExp (List [Oper Plus (Const (IntVal 1)) (Const (IntVal 2)), Const (TrueVal), Const (StringVal "yes")])],
      testCase "[x!=y,x>=y,x<=y,x not in y]" $
        parseString "[x!=y,x>=y,x<=y,x not in y]" @?=
          Right [SExp (List [Not (Oper Eq (Var "x") (Var "y")),Not (Oper Less (Var "x") (Var "y")),Not (Oper Greater (Var "x") (Var "y")),Not (Oper In (Var "x") (Var "y"))])],

      -- oper tests
      testCase "1+1" $
        parseString "1+1" @?=
          Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1-1" $
        parseString "1-1" @?=
          Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1 * 1" $
        parseString "1 * 1" @?=
          Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1 //1" $
        parseString "1 //1" @?=
          Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1% 1" $
        parseString "1% 1" @?=
          Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1 == True" $
        parseString "1 == True" @?=
          Right [SExp (Oper Eq (Const (IntVal 1)) (Const TrueVal))],
      testCase "1 != 1" $
        parseString "1 != 1" @?=
          Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 1))))],
      testCase "1 < 1" $
        parseString "1 < 1" @?=
          Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1<=1" $
        parseString "1<=1" @?=
          Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 1))))],
      testCase "1>1" $
        parseString "1>1" @?=
          Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 1)))],
      testCase "1>=1" $
        parseString "1>=1" @?=
          Right [SExp (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 1))))],
      testCase "1in [1]" $
        parseString "1in [1]" @?=
          Right [SExp (Oper In (Const (IntVal 1)) (List [Const (IntVal 1)]))],
      testCase "1 not in [1]" $
        parseString "1 not in [1]" @?=
          Right [SExp (Not (Oper In (Const (IntVal 1)) (List [Const (IntVal 1)])))],
      
      -- clausez tests
      testCase "[ 1for x in x ]" $
        parseString "[ 1for x in x ]" @?=
          Right [SExp (Compr (Const (IntVal 1)) [CCFor "x" (Var "x")])],
      testCase "[ 1 for x in x if 1 == 1]" $
        parseString "[ 1 for x in x if t == 1]" @?=
          Right [SExp (Compr (Const (IntVal 1)) [CCFor "x" (Var "x"), CCIf (Oper Eq (Var "t") (Const (IntVal 1)))])],
      testCase "[if x]" $
        case parseString "[if x]" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
 
      -- keyword space tests
      testCase "2 in[1]" $
        case parseString "2 in[1]" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,      
      testCase "[1 forx in x]" $
        case parseString "[1 forx in x]" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "[1 for x inx]" $
        case parseString "[1 for x inx]" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,
      testCase "[1for x in x ifx]" $
        case parseString "[1for x in x ifx]" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

      -- associativity tests
      testCase "1-2+3" $
        parseString "1-2+3" @?=
          Right [SExp (Oper Plus (Oper Minus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
      testCase "(1-2)+3" $
        parseString "(1-2)+3" @?=
          Right [SExp (Oper Plus (Oper Minus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
      testCase "1-(2+3)" $
        parseString "1-(2+3)" @?=
          Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 3))))],
      testCase "1*2//3" $
        parseString "1*2//3" @?=
          Right [SExp (Oper Div (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
      testCase "(1*2)//3" $
        parseString "(1*2)//3" @?=
          Right [SExp (Oper Div (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
      testCase "1*(2//3)" $
        parseString "1*(2//3)" @?=
          Right [SExp (Oper Times (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Const (IntVal 3))))],
      testCase "x<y<z" $
        case parseString "x<y<z" of
          Left e -> return ()
          Right p -> assertFailure $ "Unexpected parse: " ++ show p,

      -- statement tests
      testCase "x; x=1;1" $
        parseString "x; x=1;1" @?=
          Right [SExp (Var "x"), SDef "x" (Const (IntVal 1)), SExp (Const (IntVal 1))],

      -- comment tests
      testCase "x#bar" $
        parseString "x#bar" @?=
          Right [SExp (Var "x")],
      testCase "x#bar\\n" $
        parseString "x#bar\\n" @?=
          Right [SExp (Var "x")],
      testCase "x#\\n" $
        parseString "x#\\n" @?=
          Right [SExp (Var "x")],
      testCase "x#\\n=1" $
        parseString "x#\\nbar" @?=
          Right [SDef "x" (Const (IntVal 1))],
      testCase "x#bar 78 \\n = 1" $
        parseString "x#bar 78 \\n = 1" @?=
          Right [SDef "x" (Const (IntVal 1))]
    ]