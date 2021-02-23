module LLVM_Instruction where

import DataType2
import LLVM_Var
import BuilderState
import LLVM_Utils

import Control.Monad.State

import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type -- i64, double
import LLVM.AST.ParameterAttribute

import LLVM.AST.Constant ( Constant(GlobalReference, Int, Float) )
import LLVM.AST.CallingConvention -- call function
import LLVM.AST.AddrSpace
import LLVM.AST.Float



assign :: Identifier -> Op -> StateT Objects Maybe Operand
assign  id@(Typed str t) op = do
                                op2 <- genInstructionOperand op
                                op1 <- getPtr id
                                addInst (Do $ Store False op1 op2 Nothing 0 [])
                                var id
                                -- (n, tp) <- getLocalVar str --in case the return is a = 1
                                -- return (LocalReference tp n)

genCondInstruction :: Op -> Operand -> Operand -> Instruction
-- here if we want: if 1 then ...
genCondInstruction cond op1 op2 | t == (FloatingPointType DoubleFP) = FCmp flagf op1 op2 []
                                | t == (IntegerType 64)             = ICmp flagi op1 op2 []
                                where
                                t       = getOperandType op1
                                flagi    = genCondIFlag cond
                                flagf    = genCondFFlag cond

genCond :: Op -> Op -> Op -> StateT Objects Maybe Operand
genCond cond l r          = do
                            op1 <- genInstructionOperand l
                            op2 <- genInstructionOperand r

                            let inst = genCondInstruction cond op1 op2

                            name <- genNewName --operand name

                            addInst (name := inst)
                            
                            return (LocalReference i64 name) -- cond in i64 for now

genInstruction :: Op -> Operand -> Operand -> Instruction
genInstruction (ADD []) op1 op2 | typeop == (IntegerType 64)             = Add False False op1 op2 []
                                | typeop == (FloatingPointType DoubleFP) = FAdd noFastMathFlags op1 op2 []
                                where
                                typeop = getOperandType op1
genInstruction (MUL []) op1 op2 | typeop == (IntegerType 64)             = Mul False False op1 op2 []
                                | typeop == (FloatingPointType DoubleFP) = FMul noFastMathFlags op1 op2 []
                                where
                                typeop = getOperandType op1
genInstruction (SUB []) op1 op2 | typeop == (IntegerType 64)             = Sub False False op1 op2 []
                                | typeop == (FloatingPointType DoubleFP) = FSub noFastMathFlags op1 op2 []
                                where
                                typeop = getOperandType op1
genInstruction (DIV []) op1 op2 | typeop == (IntegerType 64)             = SDiv False op1 op2 []
                                | typeop == (FloatingPointType DoubleFP) = FDiv noFastMathFlags op1 op2 []
                                where
                                typeop = getOperandType op1


operatorInARow :: Op -> StateT Objects Maybe Operand
operatorInARow (ADD [fst])          = genInstructionOperand fst
operatorInARow (MUL [fst])          = genInstructionOperand fst
operatorInARow (DIV [fst])          = genInstructionOperand fst
operatorInARow (SUB [fst])          = genInstructionOperand fst
operatorInARow (ADD (fst:snd))      = do
                                    firstop <- genInstructionOperand fst-- get first operand
                                    secondop <- operatorInARow (ADD snd) -- get second operand
                                    let inst = genInstruction (ADD []) firstop secondop
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType firstop

                                    return (LocalReference t name)
operatorInARow (MUL (fst:snd))      = do
                                    firstop <- genInstructionOperand fst-- get first operand
                                    secondop <- operatorInARow (MUL snd) -- get second operand
                                    let inst = genInstruction (MUL []) firstop secondop
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType firstop
                                    
                                    return (LocalReference t name)
operatorInARow (SUB (fst:snd))      = do
                                    firstop <- genInstructionOperand fst-- get first operand
                                    secondop <- operatorInARow (SUB snd) -- get second operand
                                    let inst = genInstruction (SUB []) firstop secondop
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType firstop

                                    return (LocalReference t name)
operatorInARow (DIV (fst:snd))      = do
                                    firstop <- genInstructionOperand fst-- get first operand
                                    secondop <- operatorInARow (DIV snd) -- get second operand
                                    let inst = genInstruction (DIV []) firstop secondop
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType firstop
                                    
                                    return (LocalReference t name)


genInstructionOperand :: Op -> StateT Objects Maybe Operand
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
buildCallfParameter :: [Expr] -> StateT Objects Maybe ([(Operand, [ParameterAttribute])], [Type])
buildCallfParameter [xpr]       =   do 
                                    op  <- genInstructions xpr
                                    let top = getOperandType op
                                    return ([(op, [])], [top])
buildCallfParameter (expr:rest) =   do 
                                    op  <- genInstructions expr
                                    let top = getOperandType op
                                    let lbd = (\(op, t) (op2, t2) -> ([op] ++ op2, [t] ++ t2) ) ((op, []), top)
                                    functorHelper lbd <*> buildCallfParameter rest

genCallFunction :: Identifier -> [Expr] -> StateT Objects Maybe Operand
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


genRawValue :: Type -> Integer -> Operand
genRawValue (IntegerType 64) v              = (ConstantOperand (Int 64 v))
genRawValue (FloatingPointType DoubleFP) v  = (ConstantOperand (Float (Double (fromIntegral v) ) ))

genUnary :: Expr -> StateT Objects Maybe Operand
genUnary (Unary UMinus xpr)  =   do
                                op1 <- genInstructions xpr
                                let t = getOperandType op1
                                let op2 = genRawValue t (-1)
                                let inst = genInstruction (MUL []) op1 op2

                                instname <- genNewName

                                addInst (instname := inst)
                                return (LocalReference t instname)
genUnary (Unary Not xpr)     =   do
                                op <- genInstructions xpr

                                let t = getOperandType op

                                let op2 = genRawValue t 0

                                -- (Eq (XPR) (Val (Value )))
                                -- <- gencondition according to type
                                let cond = (DataType2.EQ (XPR xpr) (VAL (I 0)) )
                                let inst = genCondInstruction cond op op2 -- instruction 

                                instname <- genNewName

                                addInst (instname := inst)
                                return (LocalReference t instname)

genInstructions :: Expr -> StateT Objects Maybe Operand -- Operand
genInstructions (Val v)             = return $ getConstVal v
genInstructions (Id id)             = var id
genInstructions (Operation op)      = genInstructionOperand op
genInstructions (Callf id args)     = genCallFunction id args
genInstructions u@(Unary _ _)       = genUnary u
genInstructions (Exprs [xpr])       = genInstructions xpr
genInstructions (Exprs (xpr:rest))  = genInstructions xpr >> genInstructions (Exprs rest)
