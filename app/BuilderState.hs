module BuilderState where

-- import Control.Monad.State.Lazy
import Control.Monad.State

import Data.Map

import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type
import DataType2

data Objects = Objects { 
                        blockCount :: Integer,
                        nameCount :: Integer, -- for named instructions
                        insts :: [Named Instruction],
                        blocks :: [BasicBlock],
                        
                        lastOperand :: Maybe Operand, -- for block construction
                        
                        localVars :: Map String (Name, Type),
                        retType :: Type
                        -- globalVars :: Map String (Type)
                        }

functorHelper :: (a -> b) -> StateT Objects Maybe (a -> b)
functorHelper fct = do
                return fct

emptyObjects :: Objects
emptyObjects = Objects {
                        blockCount = 0, 
                        nameCount = 0, 
                        insts = [], 
                        blocks = [],
                        lastOperand = Nothing,
                        localVars = empty,
                        -- globalVar
                        retType = (IntegerType 64)
                        }

getCurrentBlockCount :: StateT Objects Maybe Integer
getCurrentBlockCount = do
                    o <- get
                    return (blockCount o)

increaseBlockCount :: StateT Objects Maybe ()
increaseBlockCount = do
                    o <- getCurrentBlockCount -- return IO Int

                    modify (\s -> s {blockCount = o + 1} )
                    
                    return ()

getNextBlock :: Expr -> StateT Objects Maybe Name
getNextBlock (IfThen _ _)       = do
                                newBlock <- gets blockCount
                                return (UnName $fromInteger (newBlock + 2))
getNextBlock (IfElse _ _ _)       = do
                                newBlock <- gets blockCount
                                return (UnName $fromInteger (newBlock + 3))
getNextBlock (While _ _)        = do
                                newBlock <- gets blockCount
                                return (UnName $fromInteger (newBlock + 3))
getNextBlock (For _ _ _ _)      = do
                                newBlock <- gets blockCount
                                return (UnName $fromInteger (newBlock + 3))

genNewBlockName :: Integer -> StateT Objects Maybe Name
genNewBlockName inc = do
                        curr <- getCurrentBlockCount
                        return (UnName $fromInteger (curr + inc))

genNewName :: StateT Objects Maybe Name -- Name DataCtor
genNewName = do
            nameC <- gets nameCount
            -- nameC <- gets (\s -> nameCount s )
            
            modify (\s -> s {nameCount = nameC + 1} )
            -- return (nameC + 1)
            return (UnName $fromInteger (nameC + 1))

-- construct name instruction; take his uname ref ; return it
addInst :: Named Instruction -> StateT Objects Maybe ()
addInst new_instruction = do
            instructions <- gets insts
            
            modify $(\s -> s { insts = instructions ++ [new_instruction] })
            return ()

getInsts :: StateT Objects Maybe [Named Instruction]
getInsts = do
        instructions <- gets insts
        return instructions

resetInsts :: StateT Objects Maybe ()
resetInsts = do
            s <- get
            
            modify (\s -> s {insts = []} )
            return ()

addBlock :: Named Terminator -> StateT Objects Maybe ()
addBlock term = do
                currBlock <- getCurrentBlockCount
                
                let name = (UnName $fromInteger currBlock)

                insts <- getInsts

                let b = BasicBlock name insts term

                bs <- gets blocks
                
                modify (\s -> s {blocks = bs ++ [b]} )

                resetInsts
                increaseBlockCount
                return ()

-- setLastOperand :: Operand -> StateT Objects Maybe ()
-- setLastOperand op = do
--                 -- s <- get
--                 modify (\s -> s {lastOperand = Just op } )