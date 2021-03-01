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











expectedRes12 = Just ((IfThen (Operation (VAL (I 1))) (Operation (VAL (I 5)))), "")
-- expectedRes12 = Just (IfThen (Val (I 1)) (Val (I 5)), "")
test12 = TestCase $ assertEqual "if 1 then 5" expectedRes12 (runParser parseIf "if 1 then 5")

expectedRes13 = Just (IfThen (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
-- expectedRes13 = Just (IfThen (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test13 = TestCase $ assertEqual "if 5 < 6 then 5" expectedRes13 (runParser parseIf "if 5 < 6 then 5")

expectedRes14 = Just (IfElse (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))) (Operation (VAL (I 6))), "")
-- expectedRes14 = Just (IfElse (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)) (Val (I 6)), "")
test14 = TestCase $ assertEqual "if 5 < 6 then 5 else 6" expectedRes14 (runParser parseIf "if 5 < 6 then 5 else 6")

expectedRes15 = Just (IfElse (Callf (Wait "toto") [Operation (VAL (I 5))]) (Operation (VAL (I 5))) (Operation (VAL (I 6))), "")
-- expectedRes15 = Just (IfElse (Callf (Wait "toto") [Val (I 5)]) (Val (I 5)) (Val (I 6)), "")
test15 = TestCase $ assertEqual "if toto(5) then 5 else 6" expectedRes15 (runParser parseIf "if toto(5) then 5 else 6")

expectedRes16 = Just ((While (Operation (VAL (I 1))) (Operation (VAL (I 5)))), "")
-- expectedRes16 = Just (While (Val (I 1)) (Val (I 5)), "")
test16 = TestCase $ assertEqual "while 1 do 5" expectedRes16 (runParser parseWhile "while 1 do 5")

expectedRes17 = Just (While (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
-- expectedRes17 = Just (While (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test17 = TestCase $ assertEqual "while 5 < 6 do 5" expectedRes17 (runParser parseWhile "while 5 < 6 do 5")

expectedRes18 = Just (While (Operation (DataType2.GT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
-- expectedRes18 = Just (While (Operation (DataType2.GT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test18 = TestCase $ assertEqual "while 5 > 6 do 5" expectedRes18 (runParser parseWhile "while 5 > 6 do 5")

expectedRes19 = Just (While (Callf (Wait "toto") [Operation (VAL (I 5))]) (Operation (VAL (I 5))), "")
-- expectedRes19 = Just (While (Callf (Wait "toto") [Val (I 5)]) (Val (I 5)), "")
test19 = TestCase $ assertEqual "while toto(5) do 5" expectedRes19 (runParser parseWhile "while toto(5) do 5")

expectedRes20 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (VAL (I 1))) (Operation (VAL (I 6))), "")
-- expectedRes20 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Val (I 1)) (Val (I 6)), "")
test20 = TestCase $ assertEqual "for i = 0, i < 5, 1 in 6" expectedRes20 (runParser parseFor "for i = 0, i < 5, 1 in 6")

expectedRes21 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")),VAL (I 1)]))) (Operation (VAL (I 6))), "")
-- expectedRes21 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")), VAL (I 1)]))) (Val (I 6)), "")
test21 = TestCase $ assertEqual "for i = 0, i < 5, i = i + 1 in 6" expectedRes21 (runParser parseFor "for i = 0, i < 5, i = i + 1 in 6")

expectedRes22 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")),VAL (I 1)]))) (Callf (Wait "toto") [Operation (VAL (I 5))]), "")
-- expectedRes22 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")), VAL (I 1)]))) (Callf (Wait "toto") [Val (I 5)]), "")
test22 = TestCase $ assertEqual "for i = 0, i < 5, i = i + 1 in toto(5)" expectedRes22 (runParser parseFor "for i = 0, i < 5, i = i + 1 in toto(5)")

expectedRes23 = Just (Protof (Typed "toto" INT) [Typed "x" INT] (Val (I 5)), "")
test23 = TestCase $ assertEqual "def toto(x:int):int 5;" expectedRes23 (runParser definition "def toto(x:int):int 5;")

expectedRes24 = Just (Protof (Typed "toto" INT) [Typed "x" INT, Typed "y" INT] (Callf (Wait "tata") [Val (I 5)]), "")
test24 = TestCase $ assertEqual "def toto(x : int, y : int) : int tata(5);" expectedRes24 (runParser definition "def toto(x : int, y : int) : int tata(5);")

expectedRes25 = Just (Operation (ADD [VAL (I 1),VAL (I 25)]), "")
test25 = TestCase $ assertEqual "1 + 25" expectedRes25 (runParser parseOp "1 + 25")

expectedRes26 = Just (Operation (ASSIGN (Wait "a") (ADD [VAL (I 1),VAL (I 25)])), "")
test26 = TestCase $ assertEqual "a = 1 + 25" expectedRes26 (runParser parseOp "a = 1 + 25")

expectedRes27 = Just (Operation (ADD [VAL (I 1),SUB [MUL [VAL (I 25),VAL (I 3)],MUL [VAL (I 2), SUB [VAL (I 6),VAL (I 3)]]]]), "")
test27 = TestCase $ assertEqual "1 + 25 * 3 - 2 * (6 - 3)" expectedRes27 (runParser parseOp "1 + 25 * 3 - 2 * (6 - 3)")

expectedRes28 = Just (Operation (DataType2.EQ (XPR (Unary UMinus (Val (I 5)))) (XPR (Unary Not (Unary UMinus (Val (I 2)))))), "")
test28 = TestCase $ assertEqual "-5 == !-2" expectedRes28 (runParser parseOp "5 == !2")





parsingTests = TestList [word1, word2, word3, word4, char1, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25, test26, test27, test28]

