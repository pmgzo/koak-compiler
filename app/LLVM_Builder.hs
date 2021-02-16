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
import LLVM_Block
import DataType2
import BuilderState

import Control.Monad.State


-- import qualified LLVM.AST.IntegerPredicate as IP
-- import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Map
-- import Data.String
import Data.Maybe

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
