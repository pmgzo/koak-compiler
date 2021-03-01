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
word1 = TestCase $ assertEqual "word table" expectedRes1 (runParser (word "table") "tabletop")

expectedRes2 = Just ("id", ":int):int;")
word2 = TestCase $ assertEqual "word table" expectedRes2 (runParser (word "id") "id:int):int;")

expectedRes3 = Just ("func", "(x+2)")
word3 = TestCase $ assertEqual "word table" expectedRes3 (runParser (word "func") "func(x+2)")

expectedRes4 = Just ("word", " from a sentence")
word4 = TestCase $ assertEqual "word table" expectedRes4 (runParser (word "word") "  word from a sentence")

expectedRes5 = Just ('t', "abletop")
char1 = TestCase $ assertEqual "char t abletop" expectedRes5 (runParser (char 't') "tabletop")

parsingTests = TestList [word1, word2, word3, word4, char1]