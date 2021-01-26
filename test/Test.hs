module Test where

{- it is just example of our grammar -}
-- -- def coco(double: x) double: x;

-- -- Paul
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Wait "x"))) []))

-- -- Aurele
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Typed "x" DOUBLE))) []))

-- -- y = 2.0;

-- (Xpr (Exprs [(OP1 (P (Id (Wait "y"))) [(Assign, (U (P (L (D 2.0 )))  ) ) ] )]) )

-- (Xpr (OP1 (P (Id (Typed "y" DOUBLE))) [(Assign, (OP1 (P (L (D 2.0 ))) [] ) ) ] ) )

-- -- while y < 10 do y = y * 2;

-- (Xpr (While (OP1 (P (Id (Wait "y") ) )  [(Lt, (U (P (L (I 10))) ))] ) (Exprs [ (OP1 (P (Id (Wait "y" ))) [(Assign, (OP1 (P (Id (Wait "y"))) [(Time, (U (P (L (I 2))) ) )]) )] )] )) )

-- (Xpr (While (OP1 (P (Id (Typed "y" DOUBLE) ) )  [(Lt, (U (P (L (I 10))) ))] ) (Exprs [ (OP1 (P (Id (Typed "y" DOUBLE))) [(Assign, (OP1 (P (Id (Typed "y" DOUBLE))) [(Time, (U (P (L (I 2))) ) )]) )] )] )) )


