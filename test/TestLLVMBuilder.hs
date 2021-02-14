module TestLLVMBuilder where
import Test.HUnit

import LLVM_Builder
import BuilderState
import DataType2

-- import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float

-- import qualified LLVM.AST.FloatingPointPredicate as FP


-- import Data.Maybe

-- -- resTest9ParserList = (Just ((Value (Quote "(coco)") EndList), ""))
-- -- test9ParserList = TestCase $ assertEqual "   '(coco)" resTest9ParserList (parseList "   '(coco)")

-- -- getState :: Expr -> Objects
-- -- getState input = execStateT (genInstructions input) emptyObject
-- --                 where a = do

-- -- getIOResult :: Expr -> Maybe a
-- -- getIOResult input = do
-- --     val <- execStateT (genInstructions input) emptyObjects
-- --     return val


-- testHelper :: String -> Expr -> [Named Instruction] -> Test
-- testHelper comm input expected = TestCase (do 
--                                     s <- execStateT (genInstructions input) emptyObjects

--                                     let res = insts s

--                                     assertEqual comm expected res
--                                 )

-- testHelperInstruction :: String -> Expr -> [Named Instruction] -> Test
-- testHelperInstruction comm input expected = TestCase (assertEqual comm expected res)
--                                             where
--                                             s = execStateT (genInstructions input) emptyObjects
--                                             res = insts $fromJust s


-- constIntOp :: Integer -> Operand
-- constIntOp v = (ConstantOperand (Int 64 v))

-- localIntOp :: Integer -> Operand
-- localIntOp idx = (LocalReference (IntegerType 64) (UnName $fromInteger idx))

-- input1 = (Operation (ADD [(VAL (I 5)), (VAL (I 6))]))-- expectedResult
-- expectedRes1 = [UnName 1 := Add False False (constIntOp 5) (constIntOp 6) [] ]
-- test1 = testHelperInstruction "test1" input1 expectedRes1

-- input2 = (Operation (ADD [(VAL (I 5)), (VAL (I 6)), (ADD [(VAL (I 8)), (VAL (I 0))])]))
-- expectedRes2 = [UnName 1 := Add False False (constIntOp 8) (constIntOp 0) [], 
--                 UnName 2 := Add False False (constIntOp 6) (localIntOp 1) [], 
--                 UnName 3 := Add False False (constIntOp 5) (localIntOp 2) []]

-- test2 = testHelperInstruction "test2" input2 expectedRes2

-- -- test assignment
-- -- def (a:int, b:int): a = 5: a
-- input3 = (Protof (Typed "fct1" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ASSIGN (Typed "a" INT) (VAL (I 5)) ) ), (Id (Typed "a" INT) )] ))

-- -- input4 = (Protof (Typed "fct2" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ASSIGN (Typed "a" INT) (VAL (I 5)) ) ), (Id (Typed "a" INT) )] ))

-- -- input4 = (Protof (Typed "cond1" INT) [] (Exprs [(Operation (LT (VAL (I 5)) (VAL (I 5)) ))] ) )

-- input5 = (Operation (DataType2.EQ (VAL (D 0.0)) (VAL (D 0.0)) ))

-- expectedRes5 = [UnName 1 := FCmp FP.OEQ (ConstantOperand (Float (Double 0.0 ) )) (ConstantOperand (Float (Double 0.0 ) )) [] ]

-- test5 = testHelperInstruction "test5" input5 expectedRes5



-- -- test call function
-- -- the elaborate a test suite to test the .o

-- llvmBuilderTests = TestList [test1, test2, test5]
