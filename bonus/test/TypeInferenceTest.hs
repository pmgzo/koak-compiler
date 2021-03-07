module TypeInferenceTest where
import Test.HUnit
import TypeInference
import DataType2

input1 = checkAssign [(Operation (ASSIGN (Wait "y") (VAL (I 5))))] []
expectedRes1 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))]
test1 = TestCase $ assertEqual "y = 5" expectedRes1 input1

input2 = checkAssign [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (VAL (D 6.2))))] []
expectedRes2 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" DOUBLE) (VAL (D 6.2)))]
test2 = TestCase $ assertEqual "y = 5: x = 6.2" expectedRes2 input2

input3 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (VAL (D 6.2))))]
expectedRes3 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" DOUBLE) (VAL (D 6.2)))]
test3 = TestCase $ assertEqual "y = 5: x = 6.2" expectedRes3 input3

input4 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y")))))]
expectedRes4 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" INT) (XPR (Id (Typed "y" INT))))]
test4 = TestCase $ assertEqual "y = 5: x = y" expectedRes4 input4

input5 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y"))))), (Protof (Typed "add" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))])), (Callf (Wait "add") [(Val (I 2)), (Id (Wait "x"))])]
expectedRes5 = [Operation (ASSIGN (Typed "y" INT) (VAL (I 5))),Operation (ASSIGN (Typed "x" INT) (XPR (Id (Typed "y" INT)))),Protof (Typed "add" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "a" INT))])]),Callf (Typed "add" INT) [Val (I 2),Id (Typed "x" INT)]]
test5 = TestCase $ assertEqual "y = 5; x = y; def add(a:int, b:int) a + b; add(2, y)" expectedRes5 input5

input6 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]
expectedRes6 = [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))), (Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]

test6 = TestCase $ assertEqual "y = 5: y = y * 2" expectedRes6 input6

input7 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (While (Operation (DataType2.LT (XPR (Id (Wait "y"))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]))]
expectedRes7 = [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))), (While (Operation (DataType2.LT (XPR (Id (Typed "y" INT))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]))]

test7 = TestCase $ assertEqual "y = 2; while y < 10 do y = y * 2" expectedRes7 input7

input8 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (IfThen (Operation (DataType2.LT (XPR (Id (Wait "y"))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]))]
expectedRes8 = [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))), (IfThen (Operation (DataType2.LT (XPR (Id (Typed "y" INT))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]))]

test8 = TestCase $ assertEqual "y = 2; if y < 10 then y = y * 2" expectedRes8 input8

input9 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (IfElse (Operation (DataType2.LT (XPR (Id (Wait "y"))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]))]
expectedRes9 = [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))), (IfElse (Operation (DataType2.LT (XPR (Id (Typed "y" INT))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]))]

test9 = TestCase $ assertEqual "y = 2; if y < 10 then y = y * 2 else y = y * 2" expectedRes9 input9

input10 = inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (For ((Wait "i"), (Val (I 5))) ((Wait "i"), (Val (I 10))) (Val (I 1)) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]))]
expectedRes10 = [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)))), (For ((Typed "i" INT), (Val (I 5))) ((Typed "i" INT), (Val (I 10))) (Val (I 1)) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]))]

test10 = TestCase $ assertEqual "y = 2; for i = 5: i < 10: 1 do y = y * 2" expectedRes10 input10


input11 = inferringType [(Protof (Typed "icond1" INT) [] (Exprs [(Operation (DataType2.EQ (VAL (I 0)) (VAL (I 0))))]))]
expectedRes11 = [(Protof (Typed "icond1" INT) [] (Exprs [(Operation (DataType2.EQ (VAL (I 0)) (VAL (I 0))))]))]

test11 = TestCase $ assertEqual "0 == 0" expectedRes11 input11





typeInferenceTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]
