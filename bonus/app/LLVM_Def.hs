{-# LANGUAGE OverloadedStrings #-}

module LLVM_Def where

import           LLVM.AST
import qualified LLVM.AST as AST
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import           LLVM.AST.Global
import           LLVM.AST.Linkage
import           LLVM.Context
import           LLVM.Module
import           LLVM.Target

import LLVM.AST.Type

import           Control.Monad.Except

defStrInt :: Definition
defStrInt = GlobalDefinition
          (globalVariableDefaults
           { name = Name "strInt"
           , linkage = Private
           , unnamedAddr = Just GlobalAddr
           , isConstant = True
           , LLVM.AST.Global.type' =
               ArrayType
               {nArrayElements = 4, elementType = IntegerType {typeBits = 8}}
           , initializer =
               Just
                 (C.Array
                  { C.memberType = IntegerType {typeBits = 8}
                  , C.memberValues =
                      [ C.Int {C.integerBits = 8, C.integerValue = 37}
                      , C.Int {C.integerBits = 8, C.integerValue = 100}
                      , C.Int {C.integerBits = 8, C.integerValue = 10}
                      , C.Int {C.integerBits = 8, C.integerValue = 0}
                      ]
                  })
           , LLVM.AST.Global.alignment = 1
           })

defStrDouble :: Definition
defStrDouble = GlobalDefinition
          (globalVariableDefaults
           { name = Name "strDouble"
           , linkage = Private
           , unnamedAddr = Just GlobalAddr
           , isConstant = True
           , LLVM.AST.Global.type' =
               ArrayType
               {nArrayElements = 4, elementType = IntegerType {typeBits = 8}}
           , initializer =
               Just
                 (C.Array
                  { C.memberType = IntegerType {typeBits = 8}
                  , C.memberValues =
                      [ C.Int {C.integerBits = 8, C.integerValue = 37}
                      , C.Int {C.integerBits = 8, C.integerValue = 102}
                      , C.Int {C.integerBits = 8, C.integerValue = 10}
                      , C.Int {C.integerBits = 8, C.integerValue = 0}
                      ]
                  })
           , LLVM.AST.Global.alignment = 1
           })

defPrintInt :: Definition
defPrintInt = GlobalDefinition
          (functionDefaults
           { returnType = i64
           , linkage = Private
           , name = Name "printInt"
           , parameters = ([( Parameter i64 (Name "d") []  )], False)
           , basicBlocks =
               [ BasicBlock
                   (UnName 0)
                   [ UnName 1 :=
                     Call
                     { tailCallKind = Nothing
                     , AST.callingConvention = CC.C
                     , AST.returnAttributes = []
                     , function =
                         Right
                           (ConstantOperand
                              (C.GlobalReference
                                 (PointerType
                                  { pointerReferent =
                                      FunctionType
                                      { resultType = IntegerType {typeBits = 64}
                                      , argumentTypes =
                                          [ PointerType
                                            { pointerReferent =
                                                IntegerType {typeBits = 8}
                                            , pointerAddrSpace = AddrSpace 0
                                            }
                                          ]
                                      , isVarArg = True
                                      }
                                  , pointerAddrSpace = AddrSpace 0
                                  })
                                 (Name "printf")))
                     , arguments =
                         [ ( ConstantOperand
                               (C.GetElementPtr
                                { C.inBounds = True
                                , C.address =
                                    C.GlobalReference
                                      (PointerType
                                       { pointerReferent =
                                           ArrayType
                                           { nArrayElements = 4
                                           , elementType =
                                               IntegerType {typeBits = 8}
                                           }
                                       , pointerAddrSpace = AddrSpace 0
                                       })
                                      (Name "strInt")
                                , C.indices =
                                    [ C.Int
                                      {C.integerBits = 32, C.integerValue = 0}
                                    , C.Int
                                      {C.integerBits = 32, C.integerValue = 0}
                                    ]
                                })
                           , [])
                         , 
                        ((LocalReference i64 (Name "d")), [])
                         ]
                     , AST.functionAttributes = []
                     , AST.metadata = []
                     }
                   ]
                  (Do $ Ret (Just (LocalReference i64 (Name "d")) ) [])
               ]
           })

defPrintDouble :: Definition
defPrintDouble = GlobalDefinition
          (functionDefaults
           { returnType = double
           , linkage = Private
           , name = Name "printDouble"
           , parameters = ([( Parameter double (Name "d") []  )], False)
           , basicBlocks =
               [ BasicBlock
                   (UnName 0)
                   [ UnName 1 :=
                     Call
                     { tailCallKind = Nothing
                     , AST.callingConvention = CC.C
                     , AST.returnAttributes = []
                     , function =
                         Right
                           (ConstantOperand
                              (C.GlobalReference
                                 (PointerType
                                  { pointerReferent =
                                      FunctionType
                                      { resultType = IntegerType {typeBits = 32}
                                      , argumentTypes =
                                          [ PointerType
                                            { pointerReferent =
                                                IntegerType {typeBits = 8}
                                            , pointerAddrSpace = AddrSpace 0
                                            }
                                          ]
                                      , isVarArg = True
                                      }
                                  , pointerAddrSpace = AddrSpace 0
                                  })
                                 (Name "printf")))
                     , arguments =
                         [ ( ConstantOperand
                               (C.GetElementPtr
                                { C.inBounds = True
                                , C.address =
                                    C.GlobalReference
                                      (PointerType
                                       { pointerReferent =
                                           ArrayType
                                           { nArrayElements = 4
                                           , elementType =
                                               IntegerType {typeBits = 8}
                                           }
                                       , pointerAddrSpace = AddrSpace 0
                                       })
                                      (Name "strDouble")
                                , C.indices =
                                    [ C.Int
                                      {C.integerBits = 32, C.integerValue = 0}
                                    , C.Int
                                      {C.integerBits = 32, C.integerValue = 0}
                                    ]
                                })
                           , [])
                         , ((LocalReference double (Name "d")), [])
                         ]
                     , AST.functionAttributes = []
                     , AST.metadata = []
                     }
                   ]
                  (Do $ Ret (Just (LocalReference double (Name "d")) ) [])
               ]
           })

defPrint :: Definition
defPrint = GlobalDefinition
          (functionDefaults
           { returnType = IntegerType {typeBits = 64}
           , name = Name "printf"
           , parameters =
               ( [ Parameter
                     (PointerType
                      { pointerReferent = IntegerType {typeBits = 8}
                      , pointerAddrSpace = AddrSpace 0
                      })
                     (UnName 0)
                     []
                 ]
               , True)
           })
