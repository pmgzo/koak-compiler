module LLVM_Module where

import DataType2
import LLVM_Builder

import qualified LLVM.AST as AST
import LLVM.Context
import LLVM.Module
import Data.String
import LLVM.Target
import Data.ByteString.Char8 as BS

genModule :: String -> [Expr] -> AST.Module
genModule name exprs =  AST.defaultModule {
                            AST.moduleName = fromString name,
                            AST.moduleDefinitions = genDefinitions exprs []
                        }

nameObjFile :: String -> String
nameObjFile [] = "test.o"
nameObjFile ('.':'k':'k':[]) = ".o"
nameObjFile (f:r)   = [f] ++ (nameObjFile r)

genObjFromExpr :: String -> [Expr] -> IO ()
genObjFromExpr name exprs = withContext $ \ctx -> do
                            print("get by type inference")
                            print(exprs)
                            let file  = (File (objname))
                            let mod = genModule name exprs
                            displayIR mod
                            withHostTargetMachineDefault (\machine -> (withModuleFromAST ctx mod (writeObjectToFile machine file)))
                            Prelude.putStrLn ("object file '" ++ objname ++ "' generated")
                            where
                            objname = (nameObjFile name)

displayIR :: AST.Module -> IO ()
displayIR mod = withContext $ \ctx -> do
                llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
                BS.putStrLn llvm
