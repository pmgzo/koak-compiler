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

genObjFromExpr :: String -> [Expr] -> IO ()
genObjFromExpr name exprs = withContext $ \ctx -> do
                            let file  = (File (name ++ ".o"))
                            let mod = genModule name exprs
                            displayIR mod
                            withHostTargetMachineDefault (\machine -> (withModuleFromAST ctx mod (writeObjectToFile machine file)))

                            let str = "object file \"" ++ name ++ ".o \"" ++ " generated"
                            print str

displayIR :: AST.Module -> IO ()
displayIR mod = withContext $ \ctx -> do
                llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
                BS.putStrLn llvm
