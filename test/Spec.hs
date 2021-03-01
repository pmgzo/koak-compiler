module Spec where
import Test.HUnit
import TestLLVMBuilder
import TypeInferenceTest
import TestParsing
import TestParse

main = do
    runTestTT llvmBuilderTests
    runTestTT typeInferenceTests
    runTestTT parsingTests
    runTestTT parseTests