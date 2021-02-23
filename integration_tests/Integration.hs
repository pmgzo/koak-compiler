module Integration where

import LLVM_Module
import LLVM_Builder
import DataType2
import BuilderState
import StatementHelper
import LLVM_Block
import LLVM_Var


import LLVM.AST

import Data.Maybe
import Control.Monad.State

-- import LLVM.AST.Name

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
ifFunction = (Protof (Typed "condIf" INT) [(Typed "a" INT)] (Exprs [(IfThen (Operation (DataType2.GT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Exprs [(Id (Typed "a" INT))]) )] ) )

ifElseFunction :: Expr  -- no
ifElseFunction = (Protof (Typed "condIfElse" INT) [(Typed "a" INT)] (Exprs [ifElseElem, assignElem] ) )

-- if a < 5 a = 6 else a = 5
ifElseCallBack :: Expr
ifElseCallBack = (Protof (Typed "condIECB" INT) [(Typed "a" INT)] (Exprs [(IfElse (Operation (DataType2.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Exprs [(assignA 6)]) (Exprs [(assignA 5)]) )]) )

while1 :: Expr --ok
while1 = (Protof (Typed "while1" INT) [(Typed "a" INT)] (Exprs [(whileElem [addA1]), (Id (Typed "a" INT))] ) )

ifElse2 :: Expr --ok
-- ifElse2 = (Protof (Typed "condIE2" INT) [(Typed "a" INT)] (Exprs [ifElseElem4, (Id (Typed "a" INT))]))
ifElse2 = (Protof (Typed "condIE2" INT) [(Typed "a" INT)] (Exprs [ifElseElem2, (Id (Typed "a" INT))]))

ifElse3 :: Expr -- ok -- imbrication test
ifElse3 = (Protof (Typed "condIE3" INT) [(Typed "a" INT)] (Exprs [ifElseElem5, (Id (Typed "a" INT))]) )
-- ifElse3 = (Protof (Typed "condIE3" INT) [(Typed "a" INT)] (Exprs [ifElseElem5]) )

factorial :: Expr
factorial = (Protof (Typed "fact" INT) [(Typed "a" INT)] (Exprs [ifElseFact]) )

-- ensure that condition block doesn't ret because there is another block
testImbrication1 :: Expr -- return 5 if a <= 4
testImbrication1 = (Protof (Typed "while11" INT) [(Typed "a" INT)] (Exprs [(whileCtr [ifGenericIf, addA1]), (Id (Typed "a" INT)) ] ) )


-- check if condition block ret if its the last block
testImbrication2 :: Expr -- always return 2 if a <= 2
testImbrication2 = (Protof (Typed "while12" INT) [(Typed "a" INT)] (Exprs [(whileCtr [ifGenericIf])] ))

-- if (if else) else ret?
inputImbr3 :: Expr
inputImbr3 = genericIfElse [(genericIfThen [(genericIfElse [assignA 6] [assignA 5])])] [assignA 7]

inputImbr4 :: Expr
inputImbr4 = (genericIfElse [(genericIfThen [(genericIfElse [assignA 9] [assignA 9])])] [assignA 10]) -- , (Val (I 5))

-- if 
--    if
--      if
--      else
-- else
testImbr1 = fctWrapper "testImbr2" [inputImbr3, (Val (I 5))] --ret in if else
testImbr2 = fctWrapper "testImbr3" [inputImbr4]  -- break and return att the end

--simple for
for1 = fctWrapper "for1" [(genericFor [addA1]), (Id (Typed "a" INT))]

-- while 
--   while

while13 = fctWrapper "whileImbr" [(genericWhile (condLT "a" 25) [((Operation (ASSIGN (Typed "b" INT) (VAL (I 0))))), (genericWhile (condLT "b" 5) [addB1, addA1])]), (Id (Typed "a" INT))]

globalVar1 = (Operation (ASSIGN (Typed "global1" INT) (VAL (I 7)) ))

whileGlobalVar = fctWrapper "whileImbrGlobalVar" [(genericWhile (condLT "global1" 25) [((Operation (ASSIGN (Typed "b" INT) (VAL (I 0))))), (genericWhile (condLT "b" 5) [addB1, addINT "global1"])]), (Id (Typed "global1" INT))]

getBlocks :: Expr -> [BasicBlock]
getBlocks (Protof id params (Exprs xprs)) = 
    genDefHelper $fromJust $execStateT (initState callList) emptyObjects
    where
    parameters = genProtoParameter params
    name    = getNameFromIdentifier id
    retType = getTypeFromIdentifier id
    callList = [(fillRetType retType), (addFunctionParameter params), (genCodeBlock xprs)]

main = do
    -- let bs = getBlocks ifElse3

    -- print bs
    -- genObjFromExpr "mod1" [ifElse3]

    genObjFromExpr "mod1" [
                        add, callFTest, callFTest2, callCondition, 
                        callCondition2, unaryNot, unaryMinus, ifFunction, 
                        ifElseFunction, while1, ifElseCallBack, ifElse2, ifElse3, factorial,
                        testImbrication1, testImbrication2, testImbr1, testImbr2, for1, while13,
                        globalVar1,whileGlobalVar
                        ]
    
    -- genObjFromExpr "mod1" [add, callFTest, callFTest2, callCondition, callCondition2, unaryNot, unaryMinus, ifFunction, ifElseFunction, ifElseCallBack]
   