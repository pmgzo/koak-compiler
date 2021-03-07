module LLVM_Utils where

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Type -- i64, double

import LLVM.AST.Instruction -- Add ...
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float

import LLVM.AST.Global
import LLVM.AST.Operand

import DataType
import LLVM

typeConversion :: TypeKoak -> Type
typeConversion VOID     = VoidType
typeConversion INT      = IntegerType 64
typeConversion DOUBLE   = FloatingPointType DoubleFP

getOperandType :: Operand -> Type
getOperandType (LocalReference t n)         = t
getOperandType (ConstantOperand (Int _ _))  = IntegerType 64
getOperandType (ConstantOperand (Float _))  = FloatingPointType DoubleFP

getConstantFromValue :: Value -> Constant
getConstantFromValue (I v) = (Int 64 v)
getConstantFromValue (D v) = (Float (Double v))

getConstVal :: Value -> Operand
getConstVal (I v) = (ConstantOperand (Int 64 v) )
getConstVal (D v) = (ConstantOperand (Float (Double v)) )

genCondIFlag :: Op -> IP.IntegerPredicate
genCondIFlag (DataType.EQ _ _)    = IP.EQ
genCondIFlag (DataType.NOTEQ _ _) = IP.NE
genCondIFlag (DataType.LT _ _)    = IP.SLT
genCondIFlag (DataType.GT _ _)    = IP.SGT

genCondFFlag :: Op -> FP.FloatingPointPredicate
genCondFFlag (DataType.EQ _ _)     = FP.OEQ
genCondFFlag (DataType.NOTEQ _ _)  = FP.ONE
genCondFFlag (DataType.LT _ _)     = FP.OLT
genCondFFlag (DataType.GT _ _)     = FP.OGT

compareBool :: Operand -> Instruction
compareBool op = ICmp IP.EQ op (ConstantOperand (Int 1 0) ) []