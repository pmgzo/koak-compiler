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

trickyErrorTests = TestList [test1, test2, test3, test4]
