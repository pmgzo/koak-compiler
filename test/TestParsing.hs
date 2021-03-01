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

-----------------------------
--      char and word      --
-----------------------------

expectedRes1 = Just ("table", "top")
test1 = TestCase $ assertEqual "word table" expectedRes1 (runParser (word "table") "tabletop")

expectedRes2 = Just ("id", ":int):int;")
test2 = TestCase $ assertEqual "argument" expectedRes2 (runParser (word "id") "id:int):int;")

expectedRes3 = Just ("func", "(x+2)")
test3 = TestCase $ assertEqual "function" expectedRes3 (runParser (word "func") "func(x+2)")

expectedRes4 = Just ("word", " from a sentence")
test4 = TestCase $ assertEqual "word from a sentence" expectedRes4 (runParser (word "word") "  word from a sentence")

expectedRes5 = Just ('t', "abletop")
test5 = TestCase $ assertEqual "char t abletop" expectedRes5 (runParser (char 't') "tabletop")









-----------------------------
--      for and while      --
-----------------------------

-- expectedRes12 = Just ((IfThen (Operation (VAL (I 1))) (Operation (VAL (I 5)))), "")
expectedRes12 = Just (IfThen (Val (I 1)) (Val (I 5)), "")
test12 = TestCase $ assertEqual "if 1 then 5" expectedRes12 (runParser parseIf "if 1 then 5")

-- expectedRes13 = Just (IfThen (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
expectedRes13 = Just (IfThen (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test13 = TestCase $ assertEqual "if 5 < 6 then 5" expectedRes13 (runParser parseIf "if 5 < 6 then 5")

-- expectedRes14 = Just (IfElse (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))) (Operation (VAL (I 6))), "")
expectedRes14 = Just (IfElse (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)) (Val (I 6)), "")
test14 = TestCase $ assertEqual "if 5 < 6 then 5 else 6" expectedRes14 (runParser parseIf "if 5 < 6 then 5 else 6")

-- expectedRes15 = Just (IfElse (Callf (Wait "toto") [Operation (VAL (I 5))]) (Operation (VAL (I 5))) (Operation (VAL (I 6))), "")
expectedRes15 = Just (IfElse (Callf (Wait "toto") [Val (I 5)]) (Val (I 5)) (Val (I 6)), "")
test15 = TestCase $ assertEqual "if toto(5) then 5 else 6" expectedRes15 (runParser parseIf "if toto(5) then 5 else 6")

-- expectedRes16 = Just ((While (Operation (VAL (I 1))) (Operation (VAL (I 5)))), "")
expectedRes16 = Just (While (Val (I 1)) (Val (I 5)), "")
test16 = TestCase $ assertEqual "while 1 do 5" expectedRes16 (runParser parseWhile "while 1 do 5")

-- expectedRes17 = Just (While (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
expectedRes17 = Just (While (Operation (DataType2.LT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test17 = TestCase $ assertEqual "while 5 < 6 do 5" expectedRes17 (runParser parseWhile "while 5 < 6 do 5")

-- expectedRes18 = Just (While (Operation (DataType2.GT (VAL (I 5)) (VAL (I 6)))) (Operation (VAL (I 5))), "")
expectedRes18 = Just (While (Operation (DataType2.GT (VAL (I 5)) (VAL (I 6)))) (Val (I 5)), "")
test18 = TestCase $ assertEqual "while 5 > 6 do 5" expectedRes18 (runParser parseWhile "while 5 > 6 do 5")

-- expectedRes19 = Just (While (Callf (Wait "toto") [Operation (VAL (I 5))]) (Operation (VAL (I 5))), "")
expectedRes19 = Just (While (Callf (Wait "toto") [Val (I 5)]) (Val (I 5)), "")
test19 = TestCase $ assertEqual "while toto(5) do 5" expectedRes19 (runParser parseWhile "while toto(5) do 5")

-- expectedRes20 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (VAL (I 1))) (Operation (VAL (I 6))), "")
expectedRes20 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Val (I 1)) (Val (I 6)), "")
test20 = TestCase $ assertEqual "for i = 0, i < 5, 1 in 6" expectedRes20 (runParser parseFor "for i = 0, i < 5, 1 in 6")

-- expectedRes21 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")),VAL (I 1)]))) (Operation (VAL (I 6))), "")
expectedRes21 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")), VAL (I 1)]))) (Val (I 6)), "")
test21 = TestCase $ assertEqual "for i = 0, i < 5, i = i + 1 in 6" expectedRes21 (runParser parseFor "for i = 0, i < 5, i = i + 1 in 6")

-- expectedRes22 = Just (For (Wait "i", Operation (VAL (I 0))) (Wait "i", Operation (VAL (I 5))) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")),VAL (I 1)]))) (Callf (Wait "toto") [Operation (VAL (I 5))]), "")
expectedRes22 = Just (For (Wait "i", Val (I 0)) (Wait "i", Val (I 5)) (Operation (ASSIGN (Wait "i") (ADD [XPR (Id (Wait "i")), VAL (I 1)]))) (Callf (Wait "toto") [Val (I 5)]), "")
test22 = TestCase $ assertEqual "for i = 0, i < 5, i = i + 1 in toto(5)" expectedRes22 (runParser parseFor "for i = 0, i < 5, i = i + 1 in toto(5)")


-----------------------------
--       operations        --
-----------------------------
expectedRes30 = Just (Operation (ADD [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4),VAL (I 5)]),"")
test30 = TestCase $ assertEqual "simple addition" expectedRes30 (runParser parseOp "1+2+3+4+5")

expectedRes31 = Just (Operation (MUL [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4),VAL (I 5)]),"")
test31 = TestCase $ assertEqual "simple multiplication" expectedRes31 (runParser parseOp "1*2*3*4*5")

expectedRes32 = Just (Operation (DIV [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4),VAL (I 5)]),"")
test32 = TestCase $ assertEqual "simple division" expectedRes32 (runParser parseOp "1/2/3/4/5")

expectedRes33 = Just (Operation (SUB [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4),VAL (I 5)]),"")
test33 = TestCase $ assertEqual "simple substraction" expectedRes33 (runParser parseOp "1-2-3-4-5")

expectedRes34 = Just (Operation (ADD [VAL (I 1),MUL [VAL (I 2),VAL (I 3)],DIV [VAL (I 4),VAL (I 5)]]),"")
test34 = TestCase $ assertEqual "complex signery" expectedRes34 (runParser parseOp "1+2*3+4/5")

expectedRes35 = Just (Operation (SUB [VAL (I 1),DIV [MUL [VAL (I 2),ADD [VAL (I 3),VAL (I 4)]],VAL (I 5)]]),"")
test35 = TestCase $ assertEqual "parenthesis" expectedRes35 (runParser parseOp "1-2*(3+4)/5")

expectedRes36 = Just (Operation (MUL [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4),VAL (I 5)]),"+")
test36 = TestCase $ assertEqual "extra sign" expectedRes36 (runParser parseOp "1*2*3*4*5+")

expectedRes37 = Just (Operation (MUL [VAL (I 1),VAL (I 2),VAL (I 3),VAL (I 4)]),"**5")
test37 = TestCase $ assertEqual "two signs" expectedRes37 (runParser parseOp "1*2*3*4**5")

expectedRes38 = Just (Operation (MUL [VAL (I 1),VAL (I 2),XPR (Callf (Wait "r") [Operation (DIV [VAL (I 3),VAL (I 2)])]),VAL (I 4)]),"")
test38 = TestCase $ assertEqual "function call" expectedRes38 (runParser parseOp "1*2*r(3/2)*4")

expectedRes39 = Just (Operation (ADD [XPR (Id (Wait "n")),MUL [VAL (I 1),XPR (Id (Wait "variable")),XPR (Id (Wait "other")),VAL (I 4)]]),"")
test39 = TestCase $ assertEqual "ids" expectedRes39 (runParser parseOp "n+1*variable*other*4")

expectedRes40 = Just (Operation (ADD [XPR (Id (Wait "x")),VAL (I 2)]),"")
test40 = TestCase $ assertEqual "id + spaces" expectedRes40 (runParser parseOp "x + 2")

expectedRes41 = Just (Operation (XPR (Id (Wait "whatABeautifulVariable"))),"")
test41 = TestCase $ assertEqual "One Id" expectedRes41 (runParser parseOp "whatABeautifulVariable")

-- expectedRes42 = Just (Operation (XPR (Id (Wait "whatABeautifulVariable"))),"")
expectedRes42 = Just (Id (Wait "whatABeautifulVariable"),"")
test42 = TestCase $ assertEqual "One Id main conversion" expectedRes42 (runParser parse "whatABeautifulVariable")


parsingTests = TestList [test1, test2, test3, test4, test5, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test30, test31, test32, test33, test34, test35, test36, test37, test38, test39, test40, test41, test42]