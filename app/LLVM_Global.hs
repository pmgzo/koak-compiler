module LLVM_Global where

import LLVM.AST.Global
import qualified LLVM.AST as AST
import LLVM.AST.Type
import DataType2
import LLVM_Utils
import LLVM.AST.Name

handleGlobalVariable :: Op -> (AST.Definition, (String, Type))
handleGlobalVariable (ASSIGN (Typed name INT) (VAL val))    = (def, (name, i64))
                                                where def = AST.GlobalDefinition 
                                                                globalVariableDefaults {
                                                                    name = (mkName name),
                                                                    isConstant = False,
                                                                    type' = i64,
                                                                    initializer = (Just (getConstantFromValue val))
                                                                }
handleGlobalVariable (ASSIGN (Typed name DOUBLE) (VAL val)) = (def, (name, double))
                                                where def = AST.GlobalDefinition 
                                                                globalVariableDefaults {
                                                                    name = (mkName name),
                                                                    isConstant = False,
                                                                    type' = double,
                                                                    initializer = (Just (getConstantFromValue val))
                                                                }
