module LLVM_Var where

import Data.Map

import LLVM_Utils
import BuilderState
import DataType2

import Control.Monad.State
import LLVM.AST.Operand
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Instruction -- Add ...
import Data.Maybe

import LLVM.AST.Constant ( Constant(GlobalReference, Int, Float) )


import Data.Map

getPtr :: Identifier -> StateT Objects Maybe Operand
getPtr id@(Typed name _) = do
                            glvars <- gets globalVars
                            lvars <- gets localVars
                            if member name glvars
                                then 
                                return (genVarPtr name glvars lvars)
                                else
                                getLocalPtr id

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

-- getVar :: Identifier -> StateT Objects Maybe Operand
-- getVar (Typed str _) = do
--                     let b1 = member str globalVars
--                     let b2 = member str localVars

getVar :: String -> Map String (Name, Type) 
                -> Map String (Name, Type) -> (Name, Type)
getVar name globalVars localVars    | isInGlobalVar == True = fromJust $Data.Map.lookup name globalVars
                                    | isInLocalVar == True  = fromJust $Data.Map.lookup name localVars
                                    where
                                    isInGlobalVar = member name globalVars
                                    isInLocalVar = member name localVars

genVarPtr :: String -> 
                Map String (Name, Type) -> Map String (Name, Type) -> Operand
genVarPtr n globalVars localVars  
    | isInGlobalVar == True =  let (n, t) = fromJust res in (ConstantOperand (GlobalReference (ptr t) n))
    | isInLocalVar == True = let (n, t) = fromJust res2 in (LocalReference (ptr t) n)
    where
    isInGlobalVar = member n globalVars
    isInLocalVar = member n localVars
    res = Data.Map.lookup n globalVars
    res2 = Data.Map.lookup n localVars

var :: Identifier -> StateT Objects Maybe Operand
var (Typed name t) = do
                    nameInst <- genNewName
                    lvars <- gets localVars
                    glvars <- gets globalVars
                    let (n, t) = getVar name glvars lvars

                    let pointer = genVarPtr name glvars lvars

                    let namedInst = (nameInst := Load False pointer Nothing 0 [])

                    addInst namedInst

                    return (LocalReference t nameInst)

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

-- genGlobalVarListHelper :: String -> Type -> (Name, Type)



-- genGlobalVariableList :: [(String, Type)] -> Map String (Name, Type)
-- genGlobalVariableList list = mapWithKey lbd (fromList list)
--                             where lbd = (\name -> \t -> ((mkName name), t))

-- mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
-- fromList :: Ord k => [(k, a)] -> Map k a

-- 
-- GlobalDefinition globalVariableDefaults {
--     name = 
--     isConstant = False,
--     type' = i64,
--     initializer = (Just ())
-- }
-- where 
-- name = 
-- value = 

-- handleGlobalVariable ::  -> StateT Objects Maybe ()