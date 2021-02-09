module LLVM_Builder where



import Control.Monad.State
-- import Control.Monad.State.Lazy

import LLVM.AST.Instruction -- Add ...
import LLVM.AST
import LLVM.AST.Type -- i64, double

import LLVM.AST.Constant ( Constant(GlobalReference, Int, Float) )

import LLVM.AST.Float
import LLVM.AST.Name
import LLVM.AST.Global

import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention -- call function
import LLVM.AST.ParameterAttribute

import LLVM_Utils
import LLVM_LocalVar
import LLVM_Instruction
import DataType2
import BuilderState

import Control.Monad.State

import LLVM_Block

-- import qualified LLVM.AST.IntegerPredicate as IP
-- import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Map
-- import Data.String
import Data.Maybe

genTerminator :: Type -> Named Terminator
genTerminator (IntegerType _)       = ret (Just (ConstantOperand (Int 64 0)) )
genTerminator (FloatingPointType _) = ret (Just (ConstantOperand (Float (Double 0.0)) ) )
-- void ?

handleSpecialBlock :: [Expr] -> StateT Objects Maybe ()
handleSpecialBlock [block]      = do
                                -- ret within the block
                                nextBlock <- getNextBlock block
                                
                                genSpecialBlock (nextBlock, True) block
                                
                                t <- gets retType
                                let lastStt = genTerminator t
                                addBlock lastStt -- end of if
                                return () -- return type of the function
handleSpecialBlock (block:rest) = do
                                nextBlock <- getNextBlock block

                                genSpecialBlock (nextBlock, False) block
    
                                genCodeBlock rest


genCodeBlock :: [Expr] -> StateT Objects Maybe ()
-- here is only for special block find the way so that simple instruction can also be called
genCodeBlock arr@((While _ _):_)        = handleSpecialBlock arr
genCodeBlock arr@((IfThen _ _):_)       = handleSpecialBlock arr
genCodeBlock arr@((IfElse _ _ _):_)     = handleSpecialBlock arr
genCodeBlock arr@((For _ _ _ _):_)      = handleSpecialBlock arr
genCodeBlock [e]                        = do -- for simple statement
                                        operand <- (genInstructions e)
                                        let term = ret (Just operand)
                                        addBlock term
                                        return ()
genCodeBlock (e:rest)                   = genInstructions e >> genCodeBlock rest

genDefHelper :: Objects -> [BasicBlock]
genDefHelper obj = blocks obj

genAParameter :: Identifier -> Parameter
genAParameter (Typed name tp) = Parameter t (mkName name) []
                                where
                                t = typeConversion tp

genProtoParameter :: [Identifier] -> [Parameter]
genProtoParameter []        = []
genProtoParameter [id]      = [genAParameter id]
genProtoParameter (id:ids)  = [genAParameter id] ++ genProtoParameter ids

getTypeFromIdentifier :: Identifier -> Type
getTypeFromIdentifier (Typed _ t) = typeConversion t

getNameFromIdentifier :: Identifier -> String
getNameFromIdentifier (Typed str _) = str

genDefinition :: Expr -> Definition
genDefinition (Protof id params (Exprs xprs)) = GlobalDefinition 
                                                functionDefaults
                                                {   name = (mkName name),
                                                    parameters = (parameters, False),
                                                    returnType = retType,
                                                    basicBlocks = genDefHelper $fromJust $execStateT (initState callList) emptyObjects
                                                }
                                                where
                                                parameters = genProtoParameter params
                                                name    = getNameFromIdentifier id
                                                retType = getTypeFromIdentifier id
                                                callList = [(fillRetType retType), (addFunctionParameter params), (genCodeBlock xprs)]

genDefinitions :: [Expr] -> [Definition]
genDefinitions [xpr]      = [genDefinition xpr]
genDefinitions (xpr:rest) = [genDefinition xpr] ++ genDefinitions rest
