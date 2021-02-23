module Spec where
import Test.HUnit
import TestLLVMBuilder
import TypeInferenceTest

-- resTest11ParserList  = Just ((Value T EndList), " ")
-- test11ParserList     = TestCase $ assertEqual "(#t ) " resTest11ParserList (parseList "(#t ) ")

main = do
    runTestTT llvmBuilderTests
    runTestTT typeInferenceTests
