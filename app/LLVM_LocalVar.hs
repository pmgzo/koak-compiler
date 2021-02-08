module LLVM_LocalVar where

import Data.Map

import LLVM_Utils
import BuilderState
import DataType2

import Control.Monad.State
import LLVM.AST.Operand
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import Data.Maybe


getLocalPtr :: Identifier -> StateT Objects Maybe Operand
getLocalPtr id@(Typed name tK)   = do
                            
                                    localMap <- gets localVars
                                    
                                    let isInMap = member name localMap
                                    let tC = typeConversion tK


                                    if isInMap == False
                                        then
                                        addLocalVar (name, tC)
                                        else
                                        getExistingLocalPtr id

getExistingLocalPtr :: Identifier -> StateT Objects Maybe Operand
getExistingLocalPtr (Typed name t) = do
                                    (n, t) <- getLocalVar name 
                                    return (LocalReference (ptr t) n)


localVar :: Identifier -> StateT Objects Maybe Operand
localVar (Typed str _) = do
                -- handle just local var
                nameInst <- genNewName
                (name, t) <- getLocalVar str

                let namedInst = (nameInst := Load False (LocalReference (ptr t) name) Nothing 0 [])

                addInst namedInst

                return (LocalReference t nameInst)


-- internal process

getLocalVar :: String -> StateT Objects Maybe (Name, Type)
getLocalVar str =   do
                    vars <- gets localVars
                    let a = Data.Map.lookup str vars
                    return $fromJust a


addLocalVar :: (String, Type) -> StateT Objects Maybe Operand
addLocalVar  (name, t) = do
                            localMap <- gets localVars
                            
                            let n = mkName name

                            modify (\s -> s { localVars = (insert name (n, t) localMap) } )

                            let named = (mkName name)

                            let instr = (named := Alloca t Nothing 0 [])

                            addInst instr

                            return (LocalReference (ptr t) named)

renameVar :: (String, TypeKoak) -> StateT Objects Maybe Operand
renameVar (str, tk) = do
                    let t = typeConversion tk
                    nameVar <- genNewName

                    localMap <- gets localVars
                    modify (\s -> s { localVars = (insert str (nameVar, t) localMap) } )

                    let inst = (nameVar := Alloca t Nothing 0 [])

                    addInst inst
                    return (LocalReference (ptr t) nameVar)


-- add Parameter
addFunctionParameter :: [Identifier] -> StateT Objects Maybe ()
addFunctionParameter []                     = return ()
addFunctionParameter [(Typed str tk)]       = do
                                                op <- renameVar (str, tk)
                                                let t = typeConversion tk
                                                let name = mkName str
                                                addInst (Do $ Store False op (LocalReference t name) Nothing 0 [])
                                                
addFunctionParameter ((Typed str tk):rest)  = do
                                                op <- renameVar (str, tk)
                                                let t = typeConversion tk
                                                let name = mkName str
                                                addInst (Do $ Store False op (LocalReference t name) Nothing 0 [])
                                                addFunctionParameter rest

fillRetType :: Type -> StateT Objects Maybe ()
fillRetType t = do
                (modify (\s -> s {retType = t}) )

initState :: [StateT Objects Maybe ()] -> StateT Objects Maybe ()
initState [fctState]        = fctState
initState (fctState:rest)   = do
                            fctState
                            initState rest
