module LLVM_Builder where



import Control.Monad.State

import LLVM.AST.Instruction
import LLVM.AST
import LLVM.AST.Type

import LLVM.AST.Constant ( Constant(GlobalReference, Int, Float) )

import LLVM.AST.Float
import LLVM.AST.Name
import LLVM.AST.Global

import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import LLVM.AST.ParameterAttribute

import LLVM_Utils
import LLVM_Var
import LLVM_Instruction
import LLVM_Global
import LLVM_Block
import DataType2
import BuilderState

import Control.Monad.State

import Data.Map
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

fillRetType :: Type -> StateT Objects Maybe ()
fillRetType t = do
                (modify (\s -> s {retType = t}) )

genGlobalVariableList :: [(String, Type)] -> StateT Objects Maybe ()
genGlobalVariableList list = do
                            (modify (\s -> s { globalVars = mapWithKey lbd (fromList list) } ))
                            where lbd = (\name -> \t -> ((mkName name), t))

genFunction :: Expr -> [(String, Type)] -> (Definition, [(String, Type)])
genFunction (Protof id params (Exprs xprs)) globVarList = (def, globVarList)
                                where
                                def = GlobalDefinition
                                        functionDefaults
                                        {   name = (mkName name),
                                            parameters = (parameters, False),
                                            returnType = retType,
                                            basicBlocks = genDefHelper
                                                        $fromJust $execStateT
                                                                (initState callList)
                                                                            emptyObjects
                                        }
                                parameters = genProtoParameter params
                                name    = getNameFromIdentifier id
                                retType = getTypeFromIdentifier id
                                callList = [(fillRetType retType),
                                            (addFunctionParameter params),
                                            (genGlobalVariableList globVarList),
                                                (genCodeBlock xprs)]

initState :: [StateT Objects Maybe ()] -> StateT Objects Maybe ()
initState [fctState]        = fctState
initState (fctState:rest)   = do
                            fctState
                            initState rest

genDefinition :: Expr -> [(String, Type)] -> (Definition, [(String, Type)])
genDefinition p@(Protof _ _ _) globVarList = genFunction p globVarList
genDefinition (Operation a@(ASSIGN _ _)) globVarList = concat (handleGlobalVariable a) globVarList
                                            where concat = (\(def, a) list -> (def, list ++ [a]))

genDefinitions :: [Expr] -> [(String, Type)] -> [Definition]
genDefinitions [xpr] globVarList     = [def]
                                    where
                                    res = genDefinition xpr globVarList
                                    def = fst res
genDefinitions (xpr:rest) globVarList = [def] ++ (genDefinitions rest newVarList)
                                    where
                                    res = genDefinition xpr globVarList
                                    def = fst res
                                    newVarList = snd res