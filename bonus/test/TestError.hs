module TestError where
import Test.HUnit

import DataType2

import Error

inputSum = Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "b" INT))])])

input1 = [inputSum, (Protof (Typed "fct1" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Callf (Typed "sum" INT) [Val (I 2)]]))]
expectedRes1 = [Err "call: Typed \"sum\" INTlen arg differ"]
test1 = TestCase $ assertEqual "call fct not same number of arg" expectedRes1 (findTrickyError input1)

input2 = [inputSum, (Protof (Typed "fct1" INT) [Typed "a" INT,Typed "b" INT] (Exprs [(Operation (ADD [(VAL (I 5)), (XPR (Callf (Typed "sum" INT) [Val (I 2)]))])) ]))]
expectedRes2 = [Err "call: Typed \"sum\" INTlen arg differ"]
test2 = TestCase $ assertEqual "call fct not same number of arg" expectedRes2 (findTrickyError input2)

input3 = [inputSum, (Protof (Typed "fct1" INT) [Typed "a" INT,Typed "b" INT] (Exprs [(Operation (ADD [(VAL (I 5)), (XPR (Callf (Typed "afunc" INT) [Val (I 2)]))])) ]))]
expectedRes3 = [Err "No function named Typed \"afunc\" INT"]
test3 = TestCase $ assertEqual "call fct not same number of arg" expectedRes3 (findTrickyError input3)

input4 = [
    Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "b" INT))])]),
    Protof (Typed "fact2" INT) [Typed "a" INT] (Exprs [Operation (ASSIGN (Typed "res" INT) (VAL (I 1))),While (Operation (NOTEQ (XPR (Id (Typed "a" INT))) (VAL (I 0)))) (Exprs [Operation (ASSIGN (Typed "res" INT) (MUL [XPR (Id (Typed "res" INT)),XPR (Id (Typed "a" INT))])),Operation (ASSIGN (Typed "a" INT) (SUB [XPR (Id (Typed "a" INT)),VAL (I 1)]))]),Id (Typed "res" INT)]),
    Protof (Typed "itest1" INT) [] (Exprs [Callf (Typed "sum" INT) [Callf (Typed "sum" INT) [Val (I 5),Val (I 6)],Callf (Typed "sum" INT) [Operation (SUB [XPR (Unary UMinus (Val (I 4))),VAL (I 7)])]]]),
    Protof (Typed "itest2" INT) [] (Exprs [Callf (Typed "sum" INT) [Val (I 2),Unary UMinus (Val (I 5))]]),
    Protof (Typed "itest3" INT) [] (Exprs [Callf (Typed "fact2" INT) [Val (I 1)]]),Protof (Typed "itest4" INT) [] (Exprs [Callf (Typed "fact2" INT) [Val (I 15)]]),
    Protof (Typed "dtest1" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
    Protof (Typed "dtest2" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
    Protof (Typed "dtest3" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
    Protof (Typed "dtest4" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
    Operation (ASSIGN (Typed "returnType" INT) (VAL (I 0))),
    Operation (ASSIGN (Typed "testId" INT) (VAL (I 7)))]
expectedRes4 = [Err "call: Typed \"sum\" INTlen arg differ"]
test4 = TestCase $ assertEqual "call fct not same number of arg tricky" expectedRes4 (findTrickyError input4)

protof5 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [])]

input5 = Callf (Typed "sum" INT) [(Operation (ADD [(VAL (I 1)), (VAL (I 2))])), (Operation (DIV [(VAL (I 5)), (VAL (I 5))]))]
expectedRes5 = ""
test5 = TestCase $ assertEqual "compareCall" expectedRes5 (compareCall protof5 input5)

input6 = Callf (Typed "sum" INT) [(Operation (ADD [(VAL (D 5.0)), (VAL (I 2))])), (Operation (DIV [(VAL (I 5)), (VAL (I 5))]))]
expectedRes6 = "wrong type args in call Typed \"sum\" INT"
test6 = TestCase $ assertEqual "compareCall" expectedRes6 (compareCall protof5 input6)

input7 = Callf (Typed "sum" INT) [(Unary Not (Val (I 5))), (Val (I 5))]
expectedRes7 = "wrong type args in call Typed \"sum\" INT"
test7 = TestCase $ assertEqual "compareCall" expectedRes7 (compareCall protof5 input7)

input8 = Callf (Typed "sum" INT) [(Unary UMinus (Val (I 5))), (Val (I 5))]
expectedRes8 = ""
test8 = TestCase $ assertEqual "compareCall" expectedRes7 (compareCall protof5 input7)

input9 = Callf (Typed "sum" INT) [(Unary UMinus (Val (I 5))), (Val (I 5))]
expectedRes9 = ""
test9 = TestCase $ assertEqual "compareCall" expectedRes7 (compareCall protof5 input7)

input10 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" DOUBLE] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "b" DOUBLE))])])]
expectedRes10 = [Err "INT expected but got Typed \"b\" DOUBLE"]
test10 = TestCase $ assertEqual "function's type and argument differ" expectedRes10 (findTrickyError input10)

input11 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),VAL (D 5.1)])])]
expectedRes11 = [Err "INT expected but got DOUBLE (VAL (D 5.1))"]
test11 = TestCase $ assertEqual "operator's type differ" expectedRes11 (findTrickyError input11)

input13 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [IfThen (Operation (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (D 5.1)))) (Val (I 5))])]
expectedRes13 = [Err "INT expected but got DOUBLE", Err "INT expected but got DOUBLE (VAL (D 5.1))"]
test13 = TestCase $ assertEqual "operator's type differ3" expectedRes13 (findTrickyError input13)

input12 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (I 5)))])]
expectedRes12 = [Err "Typed \"sum\" INT expected but got BOOL (Operation (EQ (XPR (Id (Typed \"a\" INT))) (VAL (I 5))))",Err "binary operation outside condition"]
test12 = TestCase $ assertEqual "bin comp in fct body" expectedRes12 (findTrickyError input12)

input14 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [VAL (I 5), (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (I 5)))])])]
expectedRes14 = [Err "binary operation outside condition"]
test14 = TestCase $ assertEqual "bin comp in fct in addition function body" expectedRes14 (findTrickyError input14)

input15 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [IfElse (Operation (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (I 5)))) (Val (I 5)) (Operation (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (I 5))))])]
expectedRes15 = [Err "binary operation outside condition"]
test15 = TestCase $ assertEqual "bin comp in function body2" expectedRes15 (findTrickyError input15)

input16 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ASSIGN (Typed "a" INT) (XPR (Unary Not (Val (I 5)))))])]
expectedRes16 = [Err "binary operation outside condition"]
test16 = TestCase $ assertEqual "operator's type differ" expectedRes16 (findTrickyError input16)

input17 = [Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [IfThen (Operation (DataType2.EQ (VAL (I 5)) (ASSIGN (Typed "a" INT) (XPR (Unary Not (Val (I 5))))) ) ) (Exprs [(Val (I 5))])])]
expectedRes17 = [Err "not operator can only be placed ahead binary comparison"]
test17 = TestCase $ assertEqual "not in assign in cond" expectedRes17 (findTrickyError input17)

input18 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (DataType2.GT (DataType2.EQ (XPR (Id (Typed "a" INT))) (VAL (I 1))) (VAL (I 7)))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
expectedRes18 = [Err "BOOL expected but got INT", Err "BOOL expected but got INT (VAL (I 7))"]
test18 = TestCase $ assertEqual "(a == 1) > 7" expectedRes18 (findTrickyError input18)

input19 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (DataType2.GT (VAL (D 1.1)) (DataType2.EQ (VAL (I 1)) (VAL (I 1))))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
expectedRes19 = [Err "DOUBLE expected but got BOOL"]
test19 = TestCase $ assertEqual "1.1 > (1 == 1)" expectedRes19 (findTrickyError input19)

input20 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (DataType2.GT (ADD [VAL (I 1),VAL (I 1)]) (DataType2.EQ (VAL (I 1)) (VAL (I 1))))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
expectedRes20 = [Err "INT expected but got BOOL"]
test20 = TestCase $ assertEqual "1+1 > (1 == 1)" expectedRes20 (findTrickyError input20)

-- input21 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (GT (VAL (D 1.1)) (EQ (VAL (I 1)) (VAL (I 1))))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
input21 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (DataType2.GT (DataType2.EQ (VAL (I 1)) (VAL (I 1))) (VAL (I 1)))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
expectedRes21 = [Err "BOOL expected but got INT", Err "BOOL expected but got INT (VAL (I 1))"]
test21 = TestCase $ assertEqual "(1 == 1) > 1" expectedRes21 (findTrickyError input21)

input22 = [Protof (Typed "fct4" INT) [Typed "a" INT] (Exprs [IfElse (Operation (DataType2.EQ (DataType2.EQ (VAL (I 1)) (VAL (I 1))) (VAL (I 1)))) (Exprs [Val (I 1)]) (Exprs [Val (I 1)])])]
expectedRes22 = [Err "BOOL expected but got INT", Err "BOOL expected but got INT (VAL (I 1))"]
test22 = TestCase $ assertEqual "(1 == 1) == 1" expectedRes22 (findTrickyError input22)

trickyErrorTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test13, test12, test14, test15, test16, test17, test18, test19, test20, test21, test22]
