module LLVM_Global where

import LLVM.AST.Global
import qualified LLVM.AST as AST
import LLVM.AST.Type
import DataType2
import LLVM_Utils
import LLVM.AST.Name

readValInt :: Expr -> Integer
readValInt (XPR (Unary UMinus (Val (I v)) ) )   = v * -1
readValInt (Val (I v))                          = v

readValDouble :: Expr -> Double
readValDouble (XPR (Unary UMinus (Val (D v)) ) ) = v * -1.0
readValDouble (Val (D v)) = v

handleGlobalVariable :: Op -> (AST.Definition, (String, Type))
handleGlobalVariable (ASSIGN (Typed name INT) v)    = (def, (name, i64))
                                                where def = AST.GlobalDefinition 
                                                                globalVariableDefaults {
                                                                    name = (mkName name),
                                                                    isConstant = False,
                                                                    type' = i64,
                                                                    initializer = (Just (getConstantFromValue val))
                                                                }
                                                where val = readValInt v
handleGlobalVariable (ASSIGN (Typed name DOUBLE) v) = (def, (name, double))
                                                where def = AST.GlobalDefinition 
                                                                globalVariableDefaults {
                                                                    name = (mkName name),
                                                                    isConstant = False,
                                                                    type' = double,
                                                                    initializer = (Just (getConstantFromValue val))
                                                                }
                                                where val = readValDouble v