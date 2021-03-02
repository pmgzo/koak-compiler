module TestParse where
import Test.HUnit

import Parse
import MyParser
import DataType2


-- input1 = callArg (Just [])

input1 = runParser parseCall "call(9, a)"
expectedRes1 = Callf (Wait "call") [(Val (I 9)), (Id (Wait "a"))] -- [Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))]
test1 = TestCase $ assertEqual "test call(9, a)" (Just (expectedRes1, "")) input1

input2 = runParser parseCall "call(9, a + a)"
expectedRes2 = Callf (Wait "call") [(Val (I 9)), (Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))] -- [Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))]
test2 = TestCase $ assertEqual "test call(9, a + a)" (Just (expectedRes2, "")) input2

-- print(show input3)
input3 = runParser parseOp "a + (a + 9) - 7"
expectedRes3 = (Operation (ADD [XPR (Id (Wait "a")),SUB [ADD [XPR (Id (Wait "a")),VAL (I 9)],VAL (I 7)]]))
test3 = TestCase $ assertEqual "test a + (a + 9) - 7" (Just (expectedRes3, "")) input3

-- infinite loop here
input4 = runParser parse ")"
expectedRes4 = Nothing
test4 = TestCase $ assertEqual "test error handling" expectedRes4 input4

input5 = runParser parseUnop "! a"
expectedRes5 = (Not)
test5 = TestCase $ assertEqual "test unop not" (Just (expectedRes5, " a")) input5

input6 = runParser parseUnop ") a"
expectedRes6 = Nothing
test6 = TestCase $ assertEqual "test error unop" expectedRes6 input6

input7 = runParser parseId "a1"
expectedRes7 = (Wait "a1")
test7 = TestCase $ assertEqual "test parseId" (Just (expectedRes7, "")) input7

input8 = runParser parseId "a"
expectedRes8 = (Wait "a")
test8 = TestCase $ assertEqual "test parseId 2" (Just (expectedRes8, "")) input8

input9 = runParser parseId "a1a1a "
expectedRes9 = (Wait "a1a1a")
test9 = TestCase $ assertEqual "test parseId 2" (Just (expectedRes9, " ")) input9

input10 = runParser parseOp "a + (a * 10)"
expectedRes10 = (Operation (ADD [XPR (Id (Wait "a")),MUL[XPR (Id (Wait "a")), VAL (I 10)]]))
test10 = TestCase $ assertEqual "test operation priority a + (a * 10)" (Just (expectedRes10, "")) input10

input11 = runParser parseOp "a * (a + 10)"
expectedRes11 = (Operation (MUL [XPR (Id (Wait "a")),ADD[XPR (Id (Wait "a")), VAL (I 10)]]))
test11 = TestCase $ assertEqual "test operation priority a * (a + 10)" (Just (expectedRes11, "")) input11

input12 = runParser parseOp "a * (a + 10) - 10 + (7 - 8) / 2"
expectedRes12 = (Operation (ADD [SUB [ MUL [(XPR (Id (Wait "a"))), (ADD [XPR (Id (Wait "a")), (VAL (I 10))])],  (VAL (I 10)) ] , DIV [SUB [(VAL (I 7)), (VAL (I 8))], (VAL (I 2))]]))
test12 = TestCase $ assertEqual "test operation priority a * (a + 10) - 10 + (7 - 8) / 2" (Just (expectedRes12, "")) input12

input13 = runParser parse "def fct1(a1:int, b2:int): int a1;"
expectedRes13 = (Protof (Typed "fct1" INT) [(Typed "a1" INT), (Typed "b2" INT)] (Exprs [(Id (Wait "a1"))]))
test13 = TestCase $ assertEqual "test id in definition" (Just (expectedRes13, "")) input13

parseTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13]
