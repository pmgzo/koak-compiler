module LLVM_Utils where

import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Type -- i64, double

import LLVM.AST.Instruction -- Add ...
import LLVM.AST.Constant ( Constant( Int, Float, GlobalReference) )
import LLVM.AST.Float

import LLVM.AST.Global
import LLVM.AST.Operand

import DataType2
import LLVM

typeConversion :: TypeKoak -> Type
typeConversion VOID     = VoidType-- not suposed to be a functipn parameter
typeConversion INT      = IntegerType 64 -- not suposed to be a parameter
typeConversion DOUBLE   = FloatingPointType DoubleFP  -- not suposed to be a parameter

getOperandType :: Operand -> Type
getOperandType (LocalReference t n)         = t
getOperandType (ConstantOperand (Int _ _))  = IntegerType 64
getOperandType (ConstantOperand (Float _))  = FloatingPointType DoubleFP

getConstVal :: Value -> Operand
getConstVal (I v) = (ConstantOperand (Int 64 v) )
getConstVal (D v) = (ConstantOperand (Float (Double v)) )

genCondIFlag :: Op -> IP.IntegerPredicate
genCondIFlag (DataType2.EQ _ _)    = IP.EQ
genCondIFlag (DataType2.NOTEQ _ _) = IP.NE
genCondIFlag (DataType2.LT _ _)    = IP.SLT
genCondIFlag (DataType2.GT _ _)    = IP.SGT

genCondFFlag :: Op -> FP.FloatingPointPredicate
genCondFFlag (DataType2.EQ _ _)     = FP.OEQ
genCondFFlag (DataType2.NOTEQ _ _)  = FP.ONE
genCondFFlag (DataType2.LT _ _)     = FP.OLT
genCondFFlag (DataType2.GT _ _)     = FP.OGT