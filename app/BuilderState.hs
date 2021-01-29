module BuilderState where

import Control.Monad.State.Lazy
import Control.Monad.State

import Data.Map

import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type

data Objects = Objects { 
                        blockCount :: Integer,
                        nameCount :: Integer,
                        insts :: [Named Instruction],
                        blocks :: [BasicBlock],
                        
                        lastOperand :: Maybe Operand, -- for block construction
                        
                        localVars :: Map String Type,
                        globalVars :: Map String Type
                        }

functorHelper :: (a -> b) -> StateT Objects IO (a -> b)
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
                        globalVars = empty
                        }

getCurrentBlockCount :: StateT Objects IO Integer
getCurrentBlockCount = do
                    o <- get
                    return (blockCount o)

increaseBlockCount :: StateT Objects IO ()
increaseBlockCount = do
                    o <- getCurrentBlockCount -- return IO Int

                    modify (\s -> s {blockCount = o + 1} )
                    
                    return ()

genNewName :: StateT Objects IO Name -- Name DataCtor
genNewName = do
            nameC <- gets nameCount
            -- nameC <- gets (\s -> nameCount s )
            
            modify (\s -> s {nameCount = nameC + 1} )
            -- return (nameC + 1)
            return (UnName $fromInteger (nameC + 1))

-- construct name instruction; take his uname ref ; return it
addInst :: Named Instruction -> StateT Objects IO ()
addInst new_instruction = do
            instructions <- gets insts
            
            modify $(\s -> s { insts = instructions ++ [new_instruction] })
            return ()

resetInsts :: StateT Objects IO ()
resetInsts = do
            s <- get
            
            modify (\s -> s {insts = []} )
            return ()

addBlock :: BasicBlock ->  StateT Objects IO ()
addBlock basicB = do
                increaseBlockCount
                -- s <- get
                bs <- gets blocks
                modify (\s -> s {blocks = bs ++ [basicB]} )
                resetInsts
                return ()

addLocalVar :: (String, Type) -> StateT Objects IO Operand
addLocalVar  (name, t) = do
                            localMap <- gets localVars

                            modify (\s -> s { localVars = (insert name t localMap) } )

                            let named = (mkName name)

                            let inst = (named := Alloca t Nothing 0 [])

                            return (LocalReference (ptr t) named)

setLastOperand :: Operand -> StateT Objects IO ()
setLastOperand op = do
                -- s <- get
                modify (\s -> s {lastOperand = Just op } )