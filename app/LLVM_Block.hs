module LLVM_Block where

import Data.Maybe
import Control.Monad.State
import LLVM.AST.Operand
import LLVM.AST.Instruction -- Add ...
import LLVM.AST.Name

import DataType2
import BuilderState
import LLVM_Instruction
-- make copy objects

-- run new state

-- run block

-- execStateT

ret :: Maybe Operand -> Named Terminator
ret op = (Do $ Ret op [])

br :: Name -> Named Terminator
br name = (Do $ Br name [])

condbr :: Operand -> Name -> Name -> Named Terminator
condbr cond n1 n2 = (Do $ CondBr cond n1 n2 [])

transmitBlockUpdate :: Objects -> StateT Objects Maybe ()
transmitBlockUpdate blockState = do
                            -- update:
                            -- get insts yes
                            -- nameCount (instruction) yes
                            -- localVars no
                            let instruction = insts blockState
                            modify (\s -> s { insts = instruction })
                            let nC = nameCount blockState
                            modify (\s -> s { nameCount = nC })
                            let bC = blockCount blockState
                            modify (\s -> s { blockCount = bC })
                            return ()

genSpeBlockTerm :: Operand -> (Name, Bool) -> StateT Objects Maybe ()
genSpeBlockTerm op (_, True)      = do
                                addBlock (Do $ Ret (Just op) [])
                                return ()
genSpeBlockTerm op (name, False)  = do
                                addBlock (Do $ Br name [])
                                return ()

genCondBlock :: Expr -> (Name, Bool) -> StateT Objects Maybe ()
genCondBlock b1 cb = do
    s <- get

    let (lastOp, s) = fromJust $runStateT (genInstructions b1) s

    transmitBlockUpdate s

    genSpeBlockTerm lastOp cb

genSpecialBlock :: (Name, Bool) -> Expr ->StateT Objects Maybe ()
genSpecialBlock cb@(name, ifend) (IfThen (Operation op) expr)   = 
    do
    nameCond <- genNewBlockName 1
    cond <- genInstructionOperand op
    let term = condbr cond nameCond (name)
    addBlock term
    genCondBlock expr cb
genSpecialBlock cb@(name, next) (IfElse (Operation op) b1 b2)   = 
    do
    
    cond <- genInstructionOperand op

    ifBlock <- genNewBlockName 1

    elseBlock <- genNewBlockName 2

    let term = condbr cond ifBlock elseBlock
    addBlock term
    genCondBlock b1 cb
    genCondBlock b2 cb
-- loop here


