module TestLLVMBuilder where
import Test.HUnit

import LLVM_Builder
import LLVM_Block
import LLVM_Instruction
import BuilderState
import DataType2

-- import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float

import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Maybe

-- resTest9ParserList = (Just ((Value (Quote "(coco)") EndList), ""))
-- test9ParserList = TestCase $ assertEqual "   '(coco)" resTest9ParserList (parseList "   '(coco)")

-- getState :: Expr -> Objects
-- getState input = execStateT (genInstructions input) emptyObject
--                 where a = do

-- getIOResult :: Expr -> Maybe a
-- getIOResult input = do
--     val <- execStateT (genInstructions input) emptyObjects
--     return val


-- testHelper :: String -> Expr -> [Named Instruction] -> Test
-- testHelper comm input expected = TestCase (do
--                                     s <- execStateT (genInstructions input) emptyObjects

--                                     let res = insts s

--                                     assertEqual comm expected res
--                                 )

testHelperInstruction :: String -> Expr -> [Named Instruction] -> Test
testHelperInstruction comm input expected = TestCase (assertEqual comm expected res)
                                            where
                                            s = execStateT (genInstructions input) emptyObjects
                                            res = insts $fromJust s


constIntOp :: Integer -> Operand
constIntOp v = (ConstantOperand (Int 64 v))

localIntOp :: Integer -> Operand
localIntOp idx = (LocalReference (IntegerType 64) (UnName $fromInteger idx))

input1 = (Operation (ADD [(VAL (I 5)), (VAL (I 6))]))-- expectedResult
expectedRes1 = [UnName 1 := Add False False (constIntOp 5) (constIntOp 6) [] ]
test1 = testHelperInstruction "test1" input1 expectedRes1

input2 = (Operation (ADD [(VAL (I 5)), (VAL (I 6)), (ADD [(VAL (I 8)), (VAL (I 0))])]))
expectedRes2 = [UnName 1 := Add False False (constIntOp 8) (constIntOp 0) [],
                UnName 2 := Add False False (constIntOp 6) (localIntOp 1) [],
                UnName 3 := Add False False (constIntOp 5) (localIntOp 2) []]

test2 = testHelperInstruction "test2" input2 expectedRes2

-- test assignment
-- def (a:int, b:int): a = 5: a
input3 = (Protof (Typed "fct1" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ASSIGN (Typed "a" INT) (VAL (I 5)) ) ), (Id (Typed "a" INT) )] ))

-- input4 = (Protof (Typed "fct2" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ASSIGN (Typed "a" INT) (VAL (I 5)) ) ), (Id (Typed "a" INT) )] ))
-- input4 = (Protof (Typed "cond1" INT) [] (Exprs [(Operation (LT (VAL (I 5)) (VAL (I 5)) ))] ) )

input5 = (Operation (DataType2.EQ (VAL (D 0.0)) (VAL (D 0.0)) ))
expectedRes5 = [UnName 1 := FCmp FP.OEQ (ConstantOperand (Float (Double 0.0 ) )) (ConstantOperand (Float (Double 0.0 ) )) [] ]
test5 = testHelperInstruction "test5" input5 expectedRes5

varA61 = (Typed "a" INT)
if62 = (IfThen (Operation (DataType2.GT (XPR (Id varA61)) (VAL (I 10)) ) ) (Exprs [(Operation (ASSIGN varA61 (VAL (I 11)) ))] ) )
exp1 = (Val (I 90))
exp2 = (Val (I 90))
input6 = (For (varA61, (Val (I 5) )) (varA61, (Val (I 10) )) (Operation (ASSIGN varA61 (ADD [(XPR (Id varA61)), (VAL (I 2)) ]))) (Exprs [if62, exp1, exp2]))
test6 = TestCase (assertEqual "test6" (scoreBlock input6) (6) )

-- ifElse3 :: Expr
-- ifElse3 = (Protof (Typed "condIE3" INT) [(Typed "a" INT)] (Exprs [ifElseElem5, (Id (Typed "a" INT))]) )

input71 = (Operation (ADD [(VAL (I 7)), (VAL (I 90))]))
input7 = (IfElse (Operation
                (DataType2.GT (XPR (Id (Typed "a" INT))) (VAL (I 5)) ))
                    (Exprs [input71, input71])
                        (Exprs [input71]))
test7 = TestCase (assertEqual "test7" (getCallbackBlock [input7]) 3)



llvmBuilderTests = TestList [test1, test2, test5, test6, test7]