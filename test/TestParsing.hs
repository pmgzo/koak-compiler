-- 
-- EPITECH PROJECT, 2020
-- B-YEP-500-PAR-5-1-koak-aurele.auboin
-- File description:
-- testParsing.hs
--

module TestParsing where
import Test.HUnit
import Parse
import DataType2

expectedRes1 = Just ("table", "top")
test1 = TestCase $ assertEqual "y = 5: x = y" expectedRes1 (runParser (word "table") "tabletop")

testList = TestList [test1]