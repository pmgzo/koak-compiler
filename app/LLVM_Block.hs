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
                            let generatedBlock = blocks blockState
                            currBlocks <- gets blocks
                            modify (\s -> s { blocks = currBlocks ++ generatedBlock })
                            return ()

genSpeBlockTerm :: Operand -> (Name, Bool) -> StateT Objects Maybe ()
genSpeBlockTerm op (_, True)      = do
                                -- to change if we use block imbrication
                                addBlock (Do $ Ret (Just op) [])
                                return ()
genSpeBlockTerm op (name, False)  = do
                                addBlock (Do $ Br name [])
                                return ()

genCondBlock :: Expr -> (Name, Bool) -> StateT Objects Maybe ()
genCondBlock b1 cb = do
    s <- get

    let (lastOp, newState) = fromJust $runStateT (genInstructions b1) s

    transmitBlockUpdate newState

    genSpeBlockTerm lastOp cb

genLoopBlock :: Expr -> Name -> StateT Objects Maybe ()
genLoopBlock xpr cb = do
    s <- get
    let (lastOp, newState) = fromJust $runStateT (genInstructions xpr) s

    transmitBlockUpdate newState

    addBlock (br cb) -- cb is the cond

handleIf :: (Name, Bool) -> Expr -> StateT Objects Maybe ()
handleIf cb@(name, ifend) (IfThen (Operation op) expr) = 
    do
    nameCond <- genNewBlockName 1
    cond <- genInstructionOperand op
    let term = condbr cond nameCond (name)
    addBlock term
    genCondBlock expr cb

handleIfElse :: (Name, Bool) -> Expr -> StateT Objects Maybe ()
handleIfElse cb@(name, next) (IfElse (Operation op) b1 b2)   = 
    do
    cond <- genInstructionOperand op
    ifBlock <- genNewBlockName 1
    elseBlock <- genNewBlockName 2
    let term = condbr cond ifBlock elseBlock
    addBlock term
    genCondBlock b1 cb
    genCondBlock b2 cb

handleWhile :: (Name, Bool) -> Expr -> StateT Objects Maybe ()
handleWhile (exitB, _) (While (Operation op) (expr)) =  -- here exprs
    do
    --close block
    nameCond <- genNewBlockName 1
    loop <- genNewBlockName 2

    addBlock (br nameCond)

    -- cond block
    cond <- genInstructionOperand op

    let term = condbr cond loop exitB

    addBlock term

    genLoopBlock expr nameCond

handleFor :: (Name, Bool) -> Expr -> StateT Objects Maybe ()
handleFor (exitB, _) (For assign@(idass, initVal) cond@(idcond, value) inc block) = 
    do
    -- carefull because if want to do block 
    -- imbrication the br block wont be call here but in the genCodeBlock
    assignBlock <- genNewBlockName 1
    condBlock <- genNewBlockName 2
    loop <- genNewBlockName 3
    previousScope <- genNewBlockName 4

    addBlock (br assignBlock)

    genInstructions (Operation (ASSIGN idass (XPR initVal)))

    addBlock (br condBlock)

    genInstructions (Operation (DataType2.LT (XPR (Id idcond)) (XPR value)) )

    addBlock (br loop)

    -- genCodeBlock [block]
    genInstructions block -- here supose to be exprs

    genInstructions inc
    -- gen inc here
    -- genCodeBlock [inc] -- instruction

    addBlock (br condBlock)

genSpecialBlock :: (Name, Bool) -> Expr ->StateT Objects Maybe ()
genSpecialBlock cb stt@(IfThen _ _)     = handleIf cb stt
genSpecialBlock cb stt@(IfElse _ _ _)   = handleIfElse cb stt
-- loop here
genSpecialBlock cb stt@(While _ _)      = handleWhile cb stt
genSpecialBlock cb stt@(For _ _ _ _)    = 
    do
    s <- get
    -- handleFor cb stt
    let (_, newState) = fromJust $runStateT (handleFor cb stt) s

    transmitBlockUpdate newState
