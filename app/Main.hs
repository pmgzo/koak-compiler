{-# LANGUAGE OverloadedStrings #-}

import Codegen
import JIT
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

{-
; ModuleID = 'my cool jit'
define double @main() {
entry:
  %1 = fadd double 1.000000e+01, 2.000000e+01
  ret double %1
}
-}

initModule :: AST.Module
initModule = emptyModule "my cool jit"

logic :: LLVM ()
logic = do
  define double "main" [] $ \ptrToMain -> do
    let a = cons $ C.Float (F.Double 10)
    let b = cons $ C.Float (F.Double 20)
    res <- fadd a b
    ret res

main :: IO AST.Module
main = do
  let ast = runLLVM initModule logic
  rc <- runJIT ast
  return ast