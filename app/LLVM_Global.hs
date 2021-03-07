module LLVM_Global where

import LLVM.AST.Global
import qualified LLVM.AST as AST
import LLVM.AST.Type
import DataType
import LLVM_Utils
import LLVM.AST.Name

readVal :: Op -> Value
readVal (XPR (Unary UMinus (Val (I v)) ) )  = (I (v * (-1)))
readVal (XPR (Unary UMinus (Val (D v))))    = (D (v * (-1.0)))
readVal (VAL v) = v

handleGlobalVariable :: Op -> (AST.Definition, (String, Type))
handleGlobalVariable (ASSIGN (Typed name INT) v)    = (def, (name, i64))
    where
    val = readVal v
    def = AST.GlobalDefinition (globalVariableDefaults {
    name = (mkName name),
    isConstant = False,
    type' = i64,
    initializer = (Just (getConstantFromValue val))
                                                            })
handleGlobalVariable (ASSIGN (Typed name DOUBLE) v) = (def, (name, double))
    where
    val = readVal v
    def = AST.GlobalDefinition (globalVariableDefaults {
    name = (mkName name),
    isConstant = False,
    type' = double,
    initializer = (Just (getConstantFromValue val))
                                                    })