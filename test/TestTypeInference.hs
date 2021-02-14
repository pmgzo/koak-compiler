module TestTypeInference where
import Test.HUnit
import TypeInference
import DataType2

input1 = checkAssign [(Operation (ASSIGN (Wait "y") (VAL (I 5))))] []
expectedRes1 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))]
test1 = TestCase $ assertEqual "y = 5" input1 expectedRes1

input2 = checkAssign [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (VAL (D 6.2))))] []
expectedRes2 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" DOUBLE) (VAL (D 6.2)))]
test2 = TestCase $ assertEqual "y = 5: x = 6.2" input2 expectedRes2

input3 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (VAL (D 6.2))))]
expectedRes3 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" DOUBLE) (VAL (D 6.2)))]
test3 = TestCase $ assertEqual "y = 5: x = 6.2" input3 expectedRes3

input4 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y")))))]
expectedRes4 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" INT) (XPR (Id (Typed "y" INT))))]
test4 = TestCase $ assertEqual "y = 5: x = y" input4 expectedRes4

input5 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y"))))), (Protof (Typed "add" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))])), (Callf (Wait "add") [(Val (I 2)), (Id (Wait "x"))])]
expectedRes5 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" INT) (XPR (Id (Typed "y" INT)))),Protof (Typed "add" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "a" INT))])]),Callf (Typed "add" INT) [Val (I 2),Id (Typed "x" INT)]]
test5 = TestCase $ assertEqual "y = 5: x = y" input5 expectedRes5



typeInferenceTests = TestList [test1, test2, test3, test4, test5]
