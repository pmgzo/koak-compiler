module Spec where
import Test.HUnit
import TestLLVMBuilder
import TypeInferenceTest

main = do
    runTestTT llvmBuilderTests
    runTestTT typeInferenceTests
