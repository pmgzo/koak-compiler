module TestLLVMBuilder where
import Test.HUnit
import LLVM_Builder
import DataType2

import Control.Monad.Trans.State.Lazy
import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )


-- resTest9ParserList = (Just ((Value (Quote "(coco)") EndList), ""))
-- test9ParserList = TestCase $ assertEqual "   '(coco)" resTest9ParserList (parseList "   '(coco)")

-- getState :: Expr -> Objects
-- getState input = execStateT (genInstructions input) emptyObject
--                 where a = do

-- getIOResult :: Expr -> Maybe a
-- getIOResult input = do
--     val <- execStateT (genInstructions input) emptyObjects
--     return val


testHelper :: String -> Expr -> [Named Instruction] -> Test
testHelper comm input expected = TestCase (do 
                                    s <- execStateT (genInstructions input) emptyObjects

                                    let res = insts s

                                    assertEqual comm expected res
                                )
constIntOp :: Integer -> Operand
constIntOp v = (ConstantOperand (Int 64 v))

localIntOp :: Integer -> Operand
localIntOp idx = (LocalReference (IntegerType 64) (UnName $fromInteger idx))

input1 = (Operation (ADD [(VAL (I 5)), (VAL (I 6))]))-- expectedResult
expectedRes1 = [UnName 1 := Add False False (constIntOp 5) (constIntOp 6) [] ]
test1 = testHelper "test1" input1 expectedRes1

input2 = (Operation (ADD [(VAL (I 5)), (VAL (I 6)), (ADD [(VAL (I 8)), (VAL (I 0))])]))
expectedRes2 = [UnName 1 := Add False False (constIntOp 8) (constIntOp 0) [], 
                UnName 2 := Add False False (constIntOp 6) (localIntOp 1) [], 
                UnName 3 := Add False False (constIntOp 5) (localIntOp 2) []]

test2 = testHelper "test2" input2 expectedRes2


llvmBuilderTests = TestList [test1, test2]
