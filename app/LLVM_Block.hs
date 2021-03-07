module LLVM_Block where

import Data.Maybe
import Control.Monad.State
import LLVM.AST.Operand
import LLVM.AST.Instruction
import LLVM.AST.Name

import LLVM.AST.Type
import LLVM.AST.Constant ( Constant(GlobalReference, Int, Float) )
import LLVM.AST.Float


import DataType2
import BuilderState
import LLVM_Instruction

ret :: Maybe Operand -> Named Terminator
ret op = (Do $ Ret op [])

br :: Name -> Named Terminator
br name = (Do $ Br name [])

condbr :: Operand -> Name -> Name -> Named Terminator
condbr cond n1 n2 = (Do $ CondBr cond n1 n2 [])

getCallbackBlock :: [Expr] -> Integer
getCallbackBlock [e] = scoreBlock e
getCallbackBlock (e:rest) = scoreBlock e + getCallbackBlock rest

scoreBlock :: Expr -> Integer
scoreBlock b@(For _ _ _(Exprs exprs))   = threshold + (getCallbackBlock exprs)
                                        where threshold     = refScore b
scoreBlock b@(While _ (Exprs exprs))    = threshold + getCallbackBlock exprs
                                        where threshold     = refScore b
scoreBlock b@(IfThen _ (Exprs exprs))   = threshold + getCallbackBlock exprs
                                        where threshold = refScore b
scoreBlock b@(IfElse _ (Exprs exprs) (Exprs exprs2)) =
    threshold + getCallbackBlock exprs + getCallbackBlock exprs2
                                    where threshold = refScore b
scoreBlock _                            = 0

refScore :: Expr -> Integer
refScore (IfThen _ _)       = 2
refScore (IfElse _ _ _)     = 3
refScore (While _ _)        = 3
refScore (For _ _ _ _)      = 4
refScore _                  = 0

getNextBlock :: Expr -> StateT Objects Maybe Name
getNextBlock xpr = do
                currentBlock <- gets blockCount
                let counter = scoreBlock xpr
                return (UnName $fromInteger (currentBlock + counter))

getNextBlockInc :: Integer -> Expr -> StateT Objects Maybe Name
getNextBlockInc inc xpr = do
                currentBlock <- gets blockCount
                let counter = scoreBlock xpr
                return (UnName $fromInteger (currentBlock + counter + inc))

getNextBlockIfElseHelper :: Integer -> StateT Objects Maybe Name
getNextBlockIfElseHelper 0 = do
                currentBlock <- gets blockCount
                return (UnName $fromInteger (currentBlock + 1 + 1))
getNextBlockIfElseHelper a = do
                currentBlock <- gets blockCount
                return (UnName $fromInteger (currentBlock + 1 + a + 1))

pop :: [a] -> [a]
pop []      = []
pop stack   = init stack

popReturn :: StateT Objects Maybe Name
popReturn = do
            stack <- gets retStack

            let infoRet = last stack

            let newStack = init stack

            modify (\s -> s { retStack = newStack })

            return (getCB infoRet)

maybePop :: StateT Objects Maybe ()
maybePop = do
            stack <- gets retStack
            let newStack = pop stack
            modify (\s -> s { retStack = newStack })

pushReturn :: InfoRet -> StateT Objects Maybe ()
pushReturn a = do
                stack <- gets retStack
                modify (\s -> s { retStack = stack ++ [a] } )

dupRetStack :: StateT Objects Maybe ()
dupRetStack = do
                stack <- gets retStack
                let infoRet = last stack
                modify (\s -> s {retStack = stack ++ [infoRet]} )

setLoopTerminator :: Name -> StateT Objects Maybe ()
setLoopTerminator name = do
                        stack <- gets retStack
                        let lastElem = last stack
                        let modifiedElem = (InfoRet (getCB lastElem) (getLastBlock lastElem) (LOOP name) )

                        modify (\s -> s {retStack = (init stack) ++ [modifiedElem]} )

getLastExitBlock :: StateT Objects Maybe Name
getLastExitBlock = do
                stack <- gets retStack
                let lastElem = last stack
                return (getCB lastElem)

handleIf :: Bool -> Expr -> StateT Objects Maybe ()
handleIf bool (IfThen op (Exprs xprs)) =
    do
    nameCond <- genNewBlockName 1
    cond <- genInstructions op
    exitB <- getLastExitBlock
    let term = condbr cond nameCond exitB

    addBlock term
    genCodeBlock xprs

handleIfElse :: Bool -> Expr -> StateT Objects Maybe ()
handleIfElse bool (IfElse op (Exprs b1) (Exprs b2))  =
    do
    cond <- genInstructions op
    ifBlock <- genNewBlockName 1
    elseBlock <- getNextBlockIfElseHelper (getCallbackBlock b1)

    let term = condbr cond ifBlock elseBlock
    addBlock term
    dupRetStack -- dup branch
    genCodeBlock b1
    genCodeBlock b2

genLoopBlock :: Expr -> Name -> StateT Objects Maybe ()
genLoopBlock (Exprs exprs) cb = do
    setLoopTerminator cb
    genCodeBlock exprs

handleWhile :: Bool -> Expr -> StateT Objects Maybe ()
handleWhile bool (While condLoop (expr)) =
    do

    nameCond <- genNewBlockName 1
    loop <- genNewBlockName 2

    addBlock (br nameCond)

    cond <- genInstructions condLoop

    exitB <- getLastExitBlock

    let term = condbr cond loop exitB

    addBlock term

    genLoopBlock expr nameCond

incrementFor :: Identifier -> Expr -> Expr
incrementFor id xpr@(Val v)                 = (Operation (ASSIGN id add))
                                            where
                                            add = (ADD [(XPR (Id id)), (XPR xpr)])
incrementFor _ xpr                          = xpr

handleFor :: Bool -> Expr -> StateT Objects Maybe ()
handleFor bool (For assign@(idass, initVal) cond@(idcond, value) inc (Exprs xprs)) =
    do

    assignBlock <- genNewBlockName 1
    condBlock <- genNewBlockName 2
    loop <- genNewBlockName 3
    previousScope <- getLastExitBlock

    addBlock (br assignBlock)

    genInstructions (Operation (ASSIGN idass (XPR initVal)))

    addBlock (br condBlock)

    condinst <- genInstructions (Operation (DataType2.LT (XPR (Id idcond)) (XPR value)) )

    addBlock (condbr condinst loop previousScope)

    let newInc = incrementFor idass inc

    genLoopBlock (Exprs (xprs ++ [newInc])) condBlock

genSpecialBlock :: Bool -> Expr ->StateT Objects Maybe ()
genSpecialBlock cb stt@(IfThen _ _)     = handleIf cb stt
genSpecialBlock cb stt@(IfElse _ _ _)   = handleIfElse cb stt
genSpecialBlock cb stt@(While _ _)      = handleWhile cb stt
genSpecialBlock cb stt@(For _ _ _ _)    = handleFor cb stt

------------------

genTerminator :: Type -> Named Terminator
genTerminator (IntegerType _)       = ret (Just (ConstantOperand (Int 64 0)) )
genTerminator (FloatingPointType _) = ret (Just (ConstantOperand (Float (Double 0.0)) ) )

handleTerm :: Bool -> Maybe Operand -> BlockId -> StateT Objects Maybe ()
handleTerm _ _ (LOOP name)      = do
                                _ <- popReturn
                                addBlock (br name)
handleTerm False _  _           = do
                                name <- popReturn
                                addBlock (br name)
handleTerm True op@(Just val) _ = do
                                addBlock (ret op)
                                maybePop
handleTerm True Nothing _       = do
                                t <- gets retType
                                let lastStt = genTerminator t
                                addBlock lastStt
                                maybePop

handleBlock :: Bool -> Expr -> StateT Objects Maybe ()
handleBlock bool xpr =  do
                    nextBlock <- getNextBlock xpr
                    currentVars <- gets localVars

                    modify (\s -> s { lastOperand = Nothing } )

                    pushReturn (InfoRet nextBlock bool NONE)

                    genSpecialBlock bool xpr

                    modify (\s -> s { localVars = currentVars } )

addEscapeBranch :: [InfoRet] -> StateT Objects Maybe ()
addEscapeBranch []   = do
                        t <- gets retType
                        let lastStt = genTerminator t
                        addBlock lastStt
addEscapeBranch (a:_) = do
                        stack <- gets retStack
                        let blockId = getBlockId stack
                        let resp = canReturn stack
                        handleTerm resp Nothing blockId

handleSpecialBlock :: [Expr] -> StateT Objects Maybe ()
handleSpecialBlock [block]      = do
                                handleBlock True block
                                stack <- gets retStack
                                addEscapeBranch stack
handleSpecialBlock (block:rest) = do
                                handleBlock False block
                                genCodeBlock rest

genCodeBlock :: [Expr] -> StateT Objects Maybe ()
genCodeBlock arr@((While _ _):_)    = handleSpecialBlock arr
genCodeBlock arr@((IfThen _ _):_)   = handleSpecialBlock arr
genCodeBlock arr@((IfElse _ _ _):_) = handleSpecialBlock arr
genCodeBlock arr@((For _ _ _ _):_)  = handleSpecialBlock arr
genCodeBlock [e]                    = do
                                    op <- (genInstructions e)
                                    stack <- gets retStack
                                    let blockId = getBlockId stack
                                    let resp = canReturn stack
                                    handleTerm resp (Just op) blockId
genCodeBlock (e:rest)               = genInstructions e >> genCodeBlock rest
