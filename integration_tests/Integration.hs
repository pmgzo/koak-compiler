module Integration where

import LLVM_Module
import LLVM_Builder
import DataType2
import BuilderState
import LLVM_Block

import Data.Maybe
import Control.Monad.State

import LLVM.AST.Name

mod1:: Expr -- ok
mod1 = (Protof (Typed "fct1" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)) ) ), (Id (Typed "y" INT) )] ))

mod2:: Expr -- ok
mod2 = (Protof (Typed "fct2" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Val (I 6))] ))

mod3:: Expr -- ok
mod3 = (Protof (Typed "fct3" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Id (Typed "a" INT) )]))

mod4 :: Expr -- ok
mod4 = (Protof (Typed "fct4" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Typed "y" INT) (ADD [(XPR (Id (Typed "a" INT))), (VAL (I 5))]) ) ), (Id (Typed "y" INT) )] ))

mod5:: Expr -- ok
mod5 = (Protof (Typed "fct5" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Typed "a" INT) (ADD [(XPR (Id (Typed "a" INT))), (VAL (I 5))]) ) ), (Id (Typed "a" INT) )] ))

add:: Expr -- ok
add = (Protof (Typed "callee" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ADD [(XPR (Id (Typed "a" INT))), (XPR (Id (Typed "b" INT)))]) )] )
            )

callFTest :: Expr -- ok
callFTest = (Protof (Typed "caller" INT) 
            []
            (Exprs [(Callf (Typed "callee" INT) [(Val (I 8)), (Val (I 5))] )] )
            )

callFTest2 :: Expr -- ok
callFTest2 = (Protof (Typed "testf" DOUBLE) 
            [(Typed "a" DOUBLE)]
            (Exprs [(Operation (ADD [(XPR (Id (Typed "a" DOUBLE))), (VAL (D 4.0))]) )] )
            )

callCondition :: Expr
callCondition = (Protof (Typed "cond1" INT) [] (Exprs [(Operation (DataType2.EQ (VAL (I 0)) (VAL (I 0)) ))] ) )

callCondition2 :: Expr
callCondition2 = (Protof (Typed "cond2" DOUBLE) [] (Exprs [(Operation (DataType2.EQ (VAL (D 9.0)) (VAL (D 0.0)) ))] ) )

unaryNot :: Expr
unaryNot = (Protof (Typed "not1" INT) [] (Exprs [(Unary Not (Val (I 0)) )] ) )

unaryMinus :: Expr 
unaryMinus = (Protof (Typed "minus1" DOUBLE) [] (Exprs [(Unary UMinus (Operation (ADD [(VAL (D 5.0)), (VAL (D 12.0))])) )] ) )

ifFunction :: Expr 
ifFunction = (Protof (Typed "condIf" INT) [(Typed "a" INT)] (Exprs [(IfThen (Operation (DataType2.GT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Id (Typed "a" INT)) )] ) )

ifElseFunction :: Expr 
ifElseFunction = (Protof (Typed "condIfElse" INT) [(Typed "a" INT)] (Exprs [(IfElse (Operation (DataType2.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Id (Typed "a" INT)) (Val (I (-11)))  )] ) )

assignA :: Integer -> Expr
assignA i = (Operation (ASSIGN (Typed "a" INT) (VAL (I i)) ) )

ifElseCallBack :: Expr
ifElseCallBack = (Protof (Typed "condIECB" INT) [(Typed "a" INT)] (Exprs [(IfElse (Operation (DataType2.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (assignA 6) (assignA 5)  ), (Id (Typed "a" INT)) ] ) )


-- while if
-- (Protof (Typed "condIECB" INT) [(Typed "a" INT)] (Exprs [(While ())] ) )

-- while (a != 5) (a = a + 1), a
-- (Exprs [(While (Operation (EQ (XPR ()) (VAL (I 5)))) ) (Operation ()) ])

main = do
    -- genObjFromExpr "mod1" [mod3]
    -- genObjFromExpr "mod1" [mod4]
    -- genObjFromExpr "mod1" [mod5]
    -- let a = (Exprs [(IfThen (Operation (DataType2.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)))) (Id (Typed "a" INT)) )] )
    -- let a = (IfThen (Operation (DataType2.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)))) (Id (Typed "a" INT)) )

    -- let a2 = fromJust $execStateT (genCodeBlock [a]) emptyObjects

    -- print(a2)
    -- let a = fromJust $execStateT (genSpecialBlock ((UnName 1), True) (Operation (DataType2.LT (VAL (I 8)) (VAL (I 5)))) ) emptyObjects
    
    -- let a = fromJust $execStateT (genSpecialBlock ((UnName 1), True) (IfThen (Operation (DataType2.LT (VAL (I 8)) (VAL (I 5)))) (Val (I 9)) ) ) emptyObjects

    -- print("a")
    genObjFromExpr "mod1" [add, callFTest, callFTest2, callCondition, callCondition2, unaryNot, unaryMinus, ifFunction, ifElseFunction, ifElseCallBack]
    -- genObjFromExpr "mod1" [add, callFTest, callFTest2, callCondition, callCondition2, unaryNot, unaryMinus, ifFunction]
    
    -- genObjFromExpr "mod1" [add, callFTest, callFTest2, callCondition, callCondition2, unaryNot, unaryMinus]
    -- genObjFromExpr "mod2" [mod2]
    -- genObjFromExpr "mod3" [mod3]
