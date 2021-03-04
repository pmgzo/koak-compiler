module CheckBool where

import DataType2

e1 :: [Expr]
e1 = [(Err "binary operation outside condition")]

e2 :: [Expr]
e2 = [(Err "not operator can only be placed ahead binary comparison")]


checkWithinNot :: [Op] -> [Expr]
checkWithinNot [] = []
checkWithinNot ((XPR xp):xs)    = checkExpNot [xp] ++ checkWithinNot xs
checkWithinNot ((ADD o):xs)     = checkWithinNot o ++ checkWithinNot xs
checkWithinNot ((SUB o):xs)     = checkWithinNot o ++ checkWithinNot xs
checkWithinNot ((DIV o):xs)     = checkWithinNot o ++ checkWithinNot xs
checkWithinNot ((MUL o):xs)     = checkWithinNot o ++ checkWithinNot xs
checkWithinNot ((DataType2.EQ o1 o2):xs)    = (checkWithinNot [o1,o2]) ++ checkWithinNot xs
checkWithinNot ((DataType2.NOTEQ o1 o2):xs) = (checkWithinNot [o1,o2]) ++ checkWithinNot xs
checkWithinNot ((DataType2.LT o1 o2):xs)    = (checkWithinNot [o1,o2]) ++ checkWithinNot xs
checkWithinNot ((DataType2.GT o1 o2):xs)    = (checkWithinNot [o1,o2]) ++ checkWithinNot xs
checkWithinNot (x:xs)           = checkWithinNot xs

checkExpNot :: [Expr] -> [Expr]
checkExpNot []                  = []
checkExpNot (a@(Unary Not xp):xs) = checkNot a ++ checkExpNot xs
checkExpNot (a:xs) = checkExpNot xs

checkNot :: Expr -> [Expr]
checkNot (Unary Not (Operation (DataType2.EQ o1 o2)))   = checkWithinNot [o1] ++ checkWithinNot [o2]
checkNot (Unary Not (Operation (DataType2.NOTEQ o1 o2)))= checkWithinNot [o1] ++ checkWithinNot [o2]
checkNot (Unary Not (Operation (DataType2.LT o1 o2)))   = checkWithinNot [o1] ++ checkWithinNot [o2]
checkNot (Unary Not (Operation (DataType2.GT o1 o2)))   = checkWithinNot [o1] ++ checkWithinNot [o2]
checkNot (Unary Not (Unary not xp))                     = checkCond xp
checkNot (Unary Not _ )                                 = e2

checkCond :: Expr -> [Expr]
checkCond u@(Unary Not o1)                      = checkNot u
checkCond (Operation (DataType2.EQ o1 o2) )     = checkWithinNot [o1] ++ checkWithinNot [o2]
checkCond (Operation (DataType2.NOTEQ o1 o2))   = checkWithinNot [o1] ++ checkWithinNot [o2]
checkCond (Operation (DataType2.LT o1 o2) )     = checkWithinNot [o1] ++ checkWithinNot [o2]
checkCond (Operation (DataType2.GT o1 o2) )     = checkWithinNot [o1] ++ checkWithinNot [o2]
checkCond _ = [(Err "no binary operation found in condition")]

checkOp :: [Op] -> [Expr]
checkOp []                          = []
checkOp ((XPR xpr):xs)             = checkExpr xpr ++ checkOp xs
checkOp ((DataType2.EQ _ _ ):xs)    = e1 ++ (checkOp xs) 
checkOp ((DataType2.NOTEQ _ _ ):xs) = e1 ++ (checkOp xs)
checkOp ((DataType2.LT _ _ ):xs)    = e1 ++ (checkOp xs)
checkOp ((DataType2.GT _ _ ):xs)    = e1 ++ (checkOp xs)
checkOp ((ADD x):xs)                = checkOp x ++ checkOp xs
checkOp ((SUB x):xs)                = checkOp x ++ checkOp xs
checkOp ((MUL x):xs)                = checkOp x ++ checkOp xs
checkOp ((DIV x):xs)                = checkOp x ++ checkOp xs
checkOp ((ASSIGN _ op):xs)          = checkOp [op] ++ checkOp xs
checkOp (x:xs)                      = checkOp xs

checkFor :: Expr -> [Expr]
checkFor (For (_, xp1) (_, xp2) xp3 xp4) = er1 ++ er2 ++ er3 ++ er4
    where
    er1 = checkExpr xp1
    er2 = checkExpr xp2
    er3 = checkExpr xp3
    er4 = checkExpr xp4

checkExpr :: Expr -> [Expr]
checkExpr (Unary Not _)     = e1
checkExpr (Operation op)    = checkOp [op]
checkExpr (Exprs xprs)      = checkBody xprs
checkExpr f@(For _ _ _ _)   = checkFor f
checkExpr _                 = []

checkBody :: [Expr] -> [Expr]
checkBody []                        = []
checkBody ((Exprs xprs):xs)         = checkBody xprs ++ checkBody xs
checkBody ((IfThen cond b):xs)      = checkCond cond ++ (checkExpr b) ++ (checkBody xs)
checkBody ((IfElse cond b1 b2):xs)  = checkCond cond ++ (checkExpr b1) ++ (checkExpr b2) ++ (checkBody xs)
checkBody ((While cond b1):xs)      = checkCond cond ++ (checkExpr b1) ++ (checkBody xs)
checkBody (a:xs)                    = checkExpr a ++ checkBody xs

checkBinOperation :: [Expr] -> [Expr]
checkBinOperation [] = []
checkBinOperation ((Protof _ _ (Exprs xprs) ):xs)               = checkBody xprs ++ checkBinOperation xs
checkBinOperation ((Operation (ASSIGN _ (XPR (Unary Not _)))):xs) = e1 ++ checkBinOperation xs
checkBinOperation (x:xs) = checkBinOperation xs
