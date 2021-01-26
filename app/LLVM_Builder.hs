module LLVM_Builder where

import Control.Monad.State
import Control.Monad.State.Lazy

import DataType2

import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type -- i64, double
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float



type GameValue = Int
type GameState = (Bool, Int)

-- https://stackoverflow.com/questions/24103108/where-is-the-data-constructor-for-state

-- State is deprecated, StateT is used instead

-- https://wiki.haskell.org/Simple_StateT_use

-- import Control.Monad.State

--
-- layer an infinite list of uniques over the IO monad
--

data Objects = Objects { blockCount :: Integer, nameCount :: Integer, insts :: [Named Instruction] }

emptyObjects :: Objects
emptyObjects = Objects {blockCount = 4, nameCount = 1, insts = []}

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

getLocalVar :: Identifier -> Operand
getLocalVar (Typed str t) = (LocalReference convT name)
                        where
                            convT = typeConversion t
                            name = mkName str

-- getConstVal (D v) = (ConstantOperand (Float (Double v)) )


-- genResolvedInstruction :: Expr -> Operand -> Operand -> Instruction
-- genResolvedInstruction (Operation (ADD [])) op1 op2 = Add False False op1 op2 []


{- to Handle Ops  -}

genInstruction :: Op -> Operand -> Operand -> Instruction
genInstruction (ADD []) op1 op2 = Add False False op1 op2 []

operatorInARow :: Op -> StateT Objects IO Operand
operatorInARow (ADD [fst])          = genInstructionOperand fst
operatorInARow (ADD (fst:snd:rest)) = do
                                    first <- genInstructionOperand fst-- get first operand
                                    snd <- operatorInARow (ADD rest) -- get second operand
                                    let inst = genInstruction (ADD []) first snd
                                    
                                    name <- genNewName --operand name
                                    
                                    addInst (name := inst)

                                    -- have to get operand type
                                    let t = getOperandType first


                                    return (LocalReference t name)

genInstructionOperand :: Op -> StateT Objects IO Operand
genInstructionOperand (VAL v)                    = return $ getConstVal v -- Constant
genInstructionOperand (XPR xpr)                  = genInstructions xpr
--Lt, Gt ...
-- Operator Add, Sub, Mul, Div
genInstructionOperand classicOp                  = operatorInARow classicOp
                                                    -- <- get first operand
                                                    -- <- get second operand

{- to Handle Exprs  -}

-- genResolvedInstruction :: Expr -> Operand -> Operand -> Instruction
-- genResolvedInstruction (Operation (ADD [_, _])) op1 op2 = Add False False op1 op2 []

genInstructions :: Expr -> StateT Objects IO Operand -- Operand
 -- Constant
genInstructions (Operation (VAL v))      = return $getConstVal v 
-- LocalRef //have to check if it exists
genInstructions (Id id)                  = return $getLocalVar id 

-- -- genInstruction (Operation (ADD [o1, o2])) = do
-- genInstruction (Operation (ADD v:ops)) = do
--                 name <- genNewName --operand name

--                 -- if 

--                 -- op1 <- (genInstruction Expr2)
--                 -- op2 <- (genInstruction Expr3)

--                 genInstructionOperator ops Nothing
                

--                 -- let inst = genResolvedInstruction xpr 
--                 let inst = genResolvedInstruction xpr 

--                 -- genResolvedInstruction ()
--                 -- genResolvedInstruction ()

--                 addInst (UnName name := inst)

--                 return (UnName name inst)

-- buildInstruction :: Expr -> StateT Objects IO ()
-- buildInstruction _ = do
--             <- 



code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    -- print y
    io $ print y
    -- print y
    -- print io
    return ()

--
-- pop the next unique off the stack
--

pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO


-- state
fct1 :: String -> Maybe (Int, String)
fct1 str = Just (4, str)

fct :: StateT [String] Maybe Int
fct = do
    -- put ["toto"]
    o <- get
    put ["carote"]

    -- print o
    return (4)







-- main :: IO ()
-- main = do
--     let s = emptyObjects

    -- runStateT increaseBlockCount s -- increaseBlockCount

    -- runStateT code [1..] >> return ()
    -- runStateT code [1..]
    -- print(s)
    -- print(a)
    -- print("here")

    -- let a = code

    -- let a = StateT { runStateT = (\s -> Just (4, s)) } -- \s -> m (a, s)

    -- -- execStateT 
    -- let val = execStateT a ["oignon"]

    -- -- runStateT val

    -- print val

    -- print "o"

