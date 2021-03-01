-- 
-- EPITECH PROJECT, 2020
-- B-YEP-500-PAR-5-1-koak-aurele.auboin
-- File description:
-- testParsing.hs
--

module TestParsing where
import Test.HUnit
import MyParser
import Parse
import DataType2

expectedRes1 = Just ("table", "top")
test1 = TestCase $ assertEqual "word table" expectedRes1 (runParser (word "table") "tabletop")

parsingTests = TestList [test1]