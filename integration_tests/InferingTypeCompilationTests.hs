module InferingTypeCompilationTests where

import DataType2

imod1:: Expr -- ok
imod1 = (Protof (Typed "fct1" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Wait "y") (VAL (I 5)) ) ), (Id (Wait "y") )] ))

imod2:: Expr -- ok
imod2 = (Protof (Typed "fct2" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Val (I 6))] ))

imod3:: Expr -- ok
imod3 = (Protof (Typed "fct3" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Id (Wait "a") )]))

imod4 :: Expr -- ok
imod4 = (Protof (Typed "fct4" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Wait "y") (ADD [(XPR (Id (Wait "a"))), (VAL (I 5))]) ) ), (Id (Wait "y"))] ))

imod5:: Expr -- ok
imod5 = (Protof (Typed "fct5" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Wait "a") (ADD [(XPR (Id (Wait "a"))), (VAL (I 5))]) ) ), (Id (Wait "a"))]))

iadd:: Expr -- ok
iadd = (Protof (Typed "iadd" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "b")))]) )] )
            )

iaddf:: Expr -- ok
iaddf = (Protof (Typed "iaddf" DOUBLE) 
            [(Typed "a" DOUBLE), (Typed "b" DOUBLE)] 
            (Exprs [(Operation (ASSIGN (Wait "y") (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "b")))]) ) ), (Id (Wait "y") )]) )


icallFTest :: Expr -- ok
icallFTest = (Protof (Typed "icaller" INT) 
            []
            (Exprs [(Callf (Wait "iadd") [(Val (I 8)), (Val (I 5))] )] )
            )

icallFTest2 :: Expr -- ok
icallFTest2 = (Protof (Typed "itestf" DOUBLE) 
            [(Typed "a" DOUBLE)]
            (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (VAL (D 4.0))]) )] )
            )

icallCondition :: Expr
icallCondition = (Protof (Typed "icond1" INT) [] (Exprs [(Operation (DataType2.EQ (VAL (I 0)) (VAL (I 0)) ))] ) )

icallCondition2 :: Expr
icallCondition2 = (Protof (Typed "cond2" DOUBLE) [] (Exprs [(Operation (DataType2.EQ (VAL (D 9.0)) (VAL (D 0.0)) ))] ) )

iunaryNot :: Expr
iunaryNot = (Protof (Typed "not1" INT) [] (Exprs [(Unary Not (Val (I 0)) )] ) )

iunaryMinus :: Expr 
iunaryMinus = (Protof (Typed "minus1" DOUBLE) [] (Exprs [(Unary UMinus (Operation (ADD [(VAL (D 5.0)), (VAL (D 12.0))])) )] ) )

iifFunction :: Expr 
iifFunction = (Protof (Typed "condIf" INT) [(Typed "a" INT)] (Exprs [(IfThen (Operation (DataType2.GT (XPR (Id (Wait "a"))) (VAL (I 5)) )) (Exprs [(Id (Wait "a"))]) )] ) )

-- iifElseFunction :: Expr  -- no
-- iifElseFunction = (Protof (Typed "condIfElse" INT) [(Typed "a" INT)] (Exprs [ifElseElem, assignElem] ) )

-- if a < 5 a = 6 else a = 5
-- iifElseCallBack :: Expr
-- iifElseCallBack = (Protof (Typed "condIECB" INT) [(Typed "a" INT)] (Exprs [(IfElse (Operation (DataType2.LT (XPR (Id (Wait "a"))) (VAL (I 5)) )) (Exprs [(assignA 6)]) (Exprs [(assignA 5)]) )]) )

-- iwhile1 :: Expr --ok
-- iwhile1 = (Protof (Typed "while1" INT) [(Typed "a" INT)] (Exprs [(whileElem [addA1]), (Id (Wait "a"))] ) )

-- iifElse2 :: Expr --ok
-- -- IifElse2 = (Protof (Typed "condIE2" INT) [(Typed "a" INT)] (Exprs [ifElseElem4, (Id (Wait "a"))]))
-- iifElse2 = (Protof (Typed "condIE2" INT) [(Typed "a" INT)] (Exprs [ifElseElem2, (Id (Wait "a"))]))

-- iifElse3 :: Expr -- ok -- imbrication test
-- iifElse3 = (Protof (Typed "condIE3" INT) [(Typed "a" INT)] (Exprs [ifElseElem5, (Id (Wait "a"))]) )
-- -- ifElse3 = (Protof (Typed "condIE3" INT) [(Typed "a" INT)] (Exprs [ifElseElem5]) )

-- ifactorial :: Expr
-- ifactorial = (Protof (Typed "fact" INT) [(Typed "a" INT)] (Exprs [ifElseFact]) )

-- -- ensure that condition block doesn't ret because there is another block
-- itestImbrication1 :: Expr -- return 5 if a <= 4
-- itestImbrication1 = (Protof (Typed "while11" INT) [(Typed "a" INT)] (Exprs [(whileCtr [ifGenericIf, addA1]), (Id (Wait "a")) ] ) )


-- -- check if condition block ret if its the last block
-- itestImbrication2 :: Expr -- always return 2 if a <= 2
-- itestImbrication2 = (Protof (Typed "while12" INT) [(Typed "a" INT)] (Exprs [(whileCtr [ifGenericIf])] ))

-- -- if (if else) else ret?
-- iinputImbr3 :: Expr
-- iinputImbr3 = genericIfElse [(genericIfThen [(genericIfElse [assignA 6] [assignA 5])])] [assignA 7]

-- iinputImbr4 :: Expr
-- iinputImbr4 = (genericIfElse [(genericIfThen [(genericIfElse [assignA 9] [assignA 9])])] [assignA 10]) -- , (Val (I 5))

-- if 
--    if
--      if
--      else
-- else
-- itestImbr1 = fctWrapper "testImbr2" [iinputImbr3, (Val (I 5))] --ret in if else
-- itestImbr2 = fctWrapper "testImbr3" [iinputImbr4]  -- break and return att the end

--simple for
-- ifor1 = fctWrapper "for1" [(genericFor [addA1]), (Id (Wait "a"))]

-- while 
--   while

-- iwhile13 = fctWrapper "whileImbr" [(genericWhile (condLT "a" 25) [((Operation (ASSIGN (Wait "b") (VAL (I 0))))), (genericWhile (condLT "b" 5) [addB1, addA1])]), (Id (Wait "a"))]

-- iglobalVar1 = (Operation (ASSIGN (Typed "global1" INT) (VAL (I 7)) ))

-- iwhileGlobalVar = fctWrapper "whileImbrGlobalVar" [(genericWhile (condLT "global1" 25) [((Operation (ASSIGN (Typed "b" INT) (VAL (I 0))))), (genericWhile (condLT "b" 5) [addB1, addINT "global1"])]), (Id (Typed "global1" INT))]


addB1 :: Expr
addB1 = (Operation (ASSIGN (Wait "b") (ADD [(XPR (Id (Wait "b"))), (VAL (I 1))] )) )

addA1 :: Expr
addA1 = (Operation (ASSIGN (Wait "a") (ADD [(XPR (Id (Wait "a"))), (VAL (I 1))] )) )

genericFor :: [Expr] -> Expr
genericFor xprs = (For ((Wait "b"), (Val (I 0))) ((Wait "b"), (Val (I 5))) (addB1) (Exprs xprs))

specialFor2 :: [Expr] -> Expr
specialFor2 xprs = (For ((Wait "b"), (Val (I 0))) ((Wait "b"), (Val (I 10))) (Val (I 1)) (Exprs xprs))

cond1 :: Expr
cond1 = (Operation (DataType2.LT (XPR (Id (Wait "a"))) (VAL (I 5)) ))

genWhile :: Expr -> [Expr] -> Expr
genWhile cond xprs = (While cond (Exprs xprs))

fctWrapper :: String -> [Expr] -> Expr
fctWrapper str xprs = (Protof (Typed str INT) [(Typed "a" INT)] (Exprs xprs))

ifor2 = fctWrapper "ifor2" [(genericFor [addA1]), (Id (Wait "a"))]

ifor3 = fctWrapper "ifor3" [(specialFor2 [addA1]), (Id (Wait "a"))]

iwhile1 = fctWrapper "iwhile1" [(genWhile cond1 [addA1]), (Id (Wait "a"))]