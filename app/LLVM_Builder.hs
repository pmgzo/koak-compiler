module LLVM_Builder where

import DataType2
import BuilderState

import Control.Monad.State
import Control.Monad.State.Lazy

import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type -- i64, double
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float
import LLVM.AST.Name

import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention -- call function
import LLVM.AST.ParameterAttribute

import Data.Map

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

-- ADD, SUB, MUL etc...
typeConversion :: TypeKoak -> Type
typeConversion VOID     = VoidType-- not suposed to be a functipn parameter
typeConversion INT      = IntegerType 64 -- not suposed to be a parameter
typeConversion DOUBLE   = FloatingPointType DoubleFP  -- not suposed to be a parameter

getOperandType :: Operand -> Type
getOperandType (LocalReference t n)         = t
getOperandType (ConstantOperand (Int _ _))  = IntegerType 64
getOperandType (ConstantOperand (Float _))  = FloatingPointType DoubleFP

getConstVal :: Value -> Operand
getConstVal (I v) = (ConstantOperand (Int 64 v) )
getConstVal (D v) = (ConstantOperand (Float (Double v)) )

getLocalVar :: Identifier -> StateT Objects IO Operand
getLocalVar (Typed str t) = do
                -- handle just local var
                let tC = typeConversion t
                name <- genNewName

                let namedInst = (name := Load False (LocalReference (ptr tC) (mkName str)) Nothing 0 [])
                addInst namedInst

                return (LocalReference tC name)

genCondIFlag :: Op -> IP.IntegerPredicate
genCondIFlag (DataType2.EQ _ _)    = IP.EQ
genCondIFlag (DataType2.NOTEQ _ _) = IP.NE
genCondIFlag (DataType2.LT _ _)    = IP.SLT
genCondIFlag (DataType2.GT _ _)    = IP.SGT

genCondFFlag :: Op -> FP.FloatingPointPredicate
genCondFFlag (DataType2.EQ _ _)     = FP.OEQ
genCondFFlag (DataType2.NOTEQ _ _)  = FP.ONE
genCondFFlag (DataType2.LT _ _)     = FP.OLT
genCondFFlag (DataType2.GT _ _)     = FP.OGT

local :: Identifier -> StateT Objects IO Operand
local (Typed name t) = do
                            localMap <- gets localVars
                            let isInMap = member name localMap

                            -- check type local var
                            let tC = typeConversion t
                            let named = (mkName name)

                            if isInMap == False
                                then
                                addLocalVar (name, tC)
                                else
                                return (LocalReference (ptr tC) named)

assign :: Identifier -> Op -> StateT Objects IO Operand
assign  id@(Typed str t) op = do
                                -- have handle global var
                                op2 <- genInstructionOperand op
                                op1 <- local id
                                
                                addInst (Do $ Store False op1 op2 Nothing 0 [])
                                
                                return op1

genCondInstruction :: Op -> Operand -> Operand -> Instruction
-- here if we want: if 1 then ...
genCondInstruction cond op1 op2 | t == (FloatingPointType DoubleFP) = FCmp flagf op1 op2 []
                                | t == (IntegerType 64)             = ICmp flagi op1 op2 []
                                where
                                t       = getOperandType op1
                                flagi    = genCondIFlag cond
                                flagf    = genCondFFlag cond

genCond :: Op -> Op -> Op -> StateT Objects IO Operand
genCond cond i1 i2          = do
                            op1 <- genInstructionOperand i1
                            op2 <- genInstructionOperand i2

                            let inst = genCondInstruction cond op1 op2

                            name <- genNewName --operand name

                            addInst (name := inst)
                            
                            return (LocalReference i64 name) -- cond in i64 for now

genInstruction :: Op -> Operand -> Operand -> Instruction
genInstruction (ADD []) op1 op2 | typeop == (IntegerType 64)             = Add False False op1 op2 []
                                | typeop == (FloatingPointType DoubleFP) = FAdd noFastMathFlags op1 op2 []
                                where
                                typeop = getOperandType op1

operatorInARow :: Op -> StateT Objects IO Operand
operatorInARow (ADD [fst])          = genInstructionOperand fst
operatorInARow (ADD (fst:snd)) = do
                                    firstop <- genInstructionOperand fst-- get first operand
                                    secondop <- operatorInARow (ADD snd) -- get second operand
                                    let inst = genInstruction (ADD []) firstop secondop
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType firstop

                                    return (LocalReference t name)

genInstructionOperand :: Op -> StateT Objects IO Operand
genInstructionOperand (VAL v)           = return $ getConstVal v -- Constant
genInstructionOperand (XPR xpr)         = genInstructions xpr
-- Function call
--Lt, Gt ...
genInstructionOperand c@(DataType2.LT i1 i2)        = genCond c i1 i2
genInstructionOperand c@(DataType2.GT i1 i2)        = genCond c i1 i2
genInstructionOperand c@(DataType2.EQ i1 i2)        = genCond c i1 i2
genInstructionOperand c@(DataType2.NOTEQ i1 i2)     = genCond c i1 i2
genInstructionOperand (ASSIGN id op)                = assign id op
-- Operator Add, Sub, Mul, Div
genInstructionOperand classicOp                     = operatorInARow classicOp


-- Callf id args
buildCallfParameter :: [Expr] -> StateT Objects IO ([(Operand, [ParameterAttribute])], [Type])
buildCallfParameter [xpr]       =   do 
                                    op  <- genInstructions xpr
                                    let top = getOperandType op
                                    return ([(op, [])], [top])
buildCallfParameter (expr:rest) =   do 
                                    op  <- genInstructions expr
                                    let top = getOperandType op
                                    let lbd = (\(op, t) (op2, t2) -> ([op] ++ op2, [t] ++ t2) ) ((op, []), top)
                                    functorHelper lbd <*> buildCallfParameter rest

genCallFunction :: Identifier -> [Expr] -> StateT Objects IO Operand
genCallFunction (Typed id t) exprs    =   do
                                        (ops, paramtyped) <- buildCallfParameter exprs
                                        instname <- genNewName
                                        
                                        let name = mkName id
                                        let fctType = typeConversion t
                                        let fctSig = (PointerType (FunctionType fctType paramtyped False) (AddrSpace 0))
                                        let inst = Call Nothing C []
                                                    (Right $ ConstantOperand $ GlobalReference fctSig name)
                                                    ops
                                                    []
                                                    []
                                        addInst (instname := inst)
                                        return (LocalReference fctType instname)

genInstructions :: Expr -> StateT Objects IO Operand -- Operand
-- Constant
genInstructions (Operation (VAL v)) = return $getConstVal v
-- LocalRef //have to check if it exists
genInstructions (Id id)             = getLocalVar id -- have to handle global
-- Operand
genInstructions (Operation op)      = genInstructionOperand op
genInstructions (Callf id args)     = genCallFunction id args

-- close block for if condition
-- closeBlock :: Maybe Name -> StateT Objects IO ()
-- closeBlock (Just name)  = do
--                         currentBlock    <- gets blockCount
--                         currentInst     <- gets insts
--                         let terminator = (Do $ Br name [])
--                         let block = BasicBlock (UnName $fromInteger currentBlock) currentInst terminator

--                         addBlock block

--                         return ()
-- closeBlock Nothing      = do
--                         currentBlock    <- gets blockCount
--                         currentInst     <- gets insts
--                         let terminator = (Do $ Ret (Just (LocalReference i64 (UnName 2)) ) [])

--                         let nameBlock = BasicBlock (UnName $fromInteger currentBlock) currentInst terminator
-- return ()

-- block handler
genCodeBlock :: Maybe Name -> [Expr] -> StateT Objects IO ()
genCodeBlock name   [xpr]                           = (genInstructions xpr) >> return ()
genCodeBlock _ ((IfThen (Operation op) expr):rest)  = do -- not done
                                                    -- and save block 
                                                    -- gen condition
                                                    condRef <- genInstructionOperand op

                                                    -- runStateT 

                                                    -- changeBlock

                                                    currentBlock <- gets blockCount
                                                    let blockIf = currentBlock + 1
                                                    let following = currentBlock + 2

                                                    -- here have to make a new block
                                                    -- (_, newState) <- (runStateT test s)

                                                    -- call function StaetT that 
                                                    -- buikd Instruction and close the block with the following

                                                    -- appelle

                                                    -- put s -- update state

                                                    -- (_, s) <- runState fct s -- second block
                                                    genCodeBlock Nothing rest
genCodeBlock _      (xpr:xprs)                      = (genInstructions xpr) >> genCodeBlock Nothing xprs
-- genCodeBlock () xpr:rest                            = do 
--                                                     op <- genInstructions xpr
--                                                     setLastOperand op
                                                    


-- terminatorName
-- buildBlock :: Maybe Name -> Expr -> StateT Objects IO ()

-- generate proto => generate function, then genCodeBlock
-- genProto :: Expr -> Definition

-- genDefintions :: [Expr] -> Module

