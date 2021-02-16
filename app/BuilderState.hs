module BuilderState where

-- import Control.Monad.State.Lazy
import Control.Monad.State

import Data.Map

import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type
import DataType2

data BlockId = LOOP Name | NONE deriving (Show)

data InfoRet = InfoRet Name Bool BlockId deriving (Show)

data Objects = Objects {
                blockCount :: Integer,
                nameCount :: Integer, -- for named instructions
                insts :: [Named Instruction],
                blocks :: [BasicBlock],
                
                -- globalVars :: Map String (Type),
                localVars :: Map String (Name, Type),
                retType :: Type,
                retStack :: [InfoRet],-- callbackLoop, isTheLastBlock, blockId
                lastOperand :: Maybe Operand

                } deriving (Show)

getCB :: InfoRet -> Name
getCB (InfoRet a _ _) = a

getLastBlock ::InfoRet -> Bool
getLastBlock (InfoRet _ b _ ) = b

-- parcoursToutEst à True
canReturn :: [InfoRet] -> Bool
canReturn []                            = True
canReturn ((InfoRet _ False _):r)       = False
canReturn ((InfoRet _ True _):r)        = canReturn r

getBlockId :: [InfoRet] -> BlockId
getBlockId []                   = NONE
-- getBlockId ((InfoRet n b bId):_) = bId
getBlockId [(InfoRet n b bId)]  = bId
getBlockId (a:rest)             = getBlockId rest

functorHelper :: (a -> b) -> StateT Objects Maybe (a -> b)
functorHelper fct = do
                return fct

emptyObjects :: Objects
emptyObjects = Objects {
                        blockCount = 0,
                        nameCount = 0,
                        insts = [],
                        blocks = [],
                        localVars = empty,
                        retType = (IntegerType 64),
                        retStack = [],
                        lastOperand = Nothing
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