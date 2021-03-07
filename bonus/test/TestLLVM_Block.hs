module TestLLVM_Block where

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


-- checkBasicBlock :: [BasicBlock] -> IO ()

-- checkBasicBlock ((BasicBlock Nom instruction term):rest) = do

-- getBasicBlockFromExpr :: Expr -> [BasicBlock]
-- test1 (Protof id params (Exprs xprs)) = genDefHelper $fromJust $execStateT (initState callList) emptyObjects
--                                         where
--                                         parameters = genProtoParameter params
--                                         name    = getNameFromIdentifier id
--                                         retType = getTypeFromIdentifier id
--                                         callList = [(fillRetType retType), (addFunctionParameter params), (genCodeBlock xprs)]

-- fctWrapper :: [Expr] -> Expr
-- fctWrapper xprs (Protof (Id (Typed "fct1" INT)) [(Typed "a" INT)] (Exprs xprs))

-- -- used as generic instruction
-- assignA :: Integer -> Expr
-- assignA val = (Operation (ASSIGN (Typed "a" INT) (VAL (I 5))))

-- -- used 
-- genericCondition :: Expr
-- genericCondition = (Operation (DataType2.EQ (XPR (Typed "a" INT)) (VAL (I 5)) ))

-- genericIfElse :: [Expr] -> [Expr] -> Expr
-- genericIfElse xprs xprs2 = (IfElse genericCondition (Exprs xprs) (Exprs xprs2))

-- genericIfThen :: [Expr] -> Expr
-- genericIfThen xprs = (IfThen genericCondition (Exprs xprs))

-- genericWhile :: [Expr] -> Expr
-- genericWhile xprs = (While genericCondition (Exprs xprs))

-- genericFor :: [Expr] -> Expr
-- genericFor xprs = (While genericCondition (Exprs xprs))

-- input11 = fctWrapper [genericWhile [(genericIfElse [assignA] [assignA])]]

-- input = 

-- while (if else)

-- while (ifelse stg) (must) ; while if else (must return)

-- for (if else) (if else)

    -- if
    --     if
    --     else
    -- else

-- if (if) else (if)



-- getBasicBlock :: [Expr] -> [BasicBlock]
-- getBasicBlock xprs = 