module Test where

{- it is just example of our grammar -}
-- -- def coco(double: x) double: x;

-- -- Paul
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Wait "x"))) []))

-- -- Aurele
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Typed "x" DOUBLE))) []))

-- -- y = 2.0;

-- (Xpr (Exprs [(OP1 (P (Id (Wait "y"))) [(Assign, (U (P (L (D 2.0))) )) ])]))

-- (Xpr (OP1 (P (Id (Typed "y" DOUBLE))) [(Assign, (OP1 (P (L (D 2.0))) [])) ]))

-- -- while y < 10 do y = y * 2;


-- (While (Operation (DataType2.LT (XPR (Id (Wait "y"))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Wait "y") (MUL [(XPR (Id (Wait "y"))), (VAL (I 2))])))]))

-- (While (Operation (DataType2.LT (XPR (Id (Typed "y" INT))) (VAL (I 10)))) (Exprs [(Operation (ASSIGN (Typed "y" INT) (MUL [(XPR (Id (Typed "y" INT))), (VAL (I 2))])))]))
