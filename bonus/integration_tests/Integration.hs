module Integration where

import CompilerTests
import InferingTypeCompilationTests

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

import TypeInference

getBlocks :: Expr -> [BasicBlock]
getBlocks (Protof id params (Exprs xprs)) = 
    genDefHelper $fromJust $execStateT (initState callList) emptyObjects
    where
    parameters = genProtoParameter params
    name    = getNameFromIdentifier id
    retType = getTypeFromIdentifier id
    callList = [(fillRetType retType), (addFunctionParameter params), (genCodeBlock xprs)]

tmp1 = Protof (Typed "tmp1" DOUBLE) [Typed "break" DOUBLE] (Exprs [IfElse (Operation (DataType2.EQ (XPR (Id (Wait "break"))) (VAL (D 1.0)))) (Exprs [Val (I 5)]) (Exprs [Val (I 6)])])

main = do
    
    genObjFromExpr True "mod2" $inferringType [imod1,
                        iadd, icallFTest, icallFTest2, icallCondition, iaddf, ifor2, iwhile1, ifor3, tmp1]-- 
                        -- icallCondition2, iunaryNot, iunaryMinus, iifFunction, 
                        -- iifElseFunction, iwhile1, iifElseCallBack, iifElse2, iifElse3, ifactorial,
                        -- itestImbrication1, itestImbrication2, itestImbr1, itestImbr2, ifor1, iwhile13,
                        -- iglobalVar1, iwhileGlobalVar

    -- let bs = getBlocks ifElse3

    -- print bs
    -- genObjFromExpr "mod1" [ifElse3]

    -- compiler tests
    -- genObjFromExpr "mod1" [
    --                     add, addf, callFTest, callFTest2, callCondition, 
    --                     callCondition2, unaryNot, unaryMinus, ifFunction, 
    --                     ifElseFunction, while1, ifElseCallBack, ifElse2, ifElse3, factorial,
    --                     testImbrication1, testImbrication2, testImbr1, testImbr2, for1, while13,
    --                     globalVar1,whileGlobalVar
    --                     ]
    
    -- genObjFromExpr "mod1" [add, callFTest, callFTest2, callCondition, callCondition2, unaryNot, unaryMinus, ifFunction, ifElseFunction, ifElseCallBack]
   