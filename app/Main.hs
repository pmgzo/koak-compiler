import System.Exit (exitSuccess, exitWith)
import System.Exit
import LLVM_Module
import TypeInference
import DataType2

main :: IO ()
main = do
  let exprs = inferringType []--functionPaul
  case exprs of
    [(Err str)] -> exitWith (ExitFailure 84)
    []  -> print("empty array") >> exitWith (ExitFailure 84)
    exprs2 -> genObjFromExpr "obj" exprs2

  print("here")