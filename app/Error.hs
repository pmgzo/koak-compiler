module Error where

import DataType2

-- a :: [Expr] 
-- a = [
--     Protof (Typed "sum" INT) [Typed "a" INT,Typed "b" INT] (Exprs [Operation (ADD [XPR (Id (Typed "a" INT)),XPR (Id (Typed "b" INT))])]),
--     Protof (Typed "fact2" INT) [Typed "a" INT] (Exprs [Operation (ASSIGN (Typed "res" INT) (VAL (I 1))),While (Operation (NOTEQ (XPR (Id (Typed "a" INT))) (VAL (I 0)))) (Exprs [Operation (ASSIGN (Typed "res" INT) (MUL [XPR (Id (Typed "res" INT)),XPR (Id (Typed "a" INT))])),Operation (ASSIGN (Typed "a" INT) (SUB [XPR (Id (Typed "a" INT)),VAL (I 1)]))]),Id (Typed "res" INT)]),
--     Protof (Typed "itest1" INT) [] (Exprs [Callf (Typed "sum" INT) [Callf (Typed "sum" INT) [Val (I 5),Val (I 6)],Callf (Typed "sum" INT) [Operation (SUB [XPR (Unary UMinus (Val (I 4))),VAL (I 7)])]]]),
--     Protof (Typed "itest2" INT) [] (Exprs [Callf (Typed "sum" INT) [Val (I 2),Unary UMinus (Val (I 5))]]),
--     Protof (Typed "itest3" INT) [] (Exprs [Callf (Typed "fact2" INT) [Val (I 1)]]),Protof (Typed "itest4" INT) [] (Exprs [Callf (Typed "fact2" INT) [Val (I 15)]]),
--     Protof (Typed "dtest1" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
--     Protof (Typed "dtest2" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
--     Protof (Typed "dtest3" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
--     Protof (Typed "dtest4" DOUBLE) [] (Exprs [Unary UMinus (Val (D 1.0))]),
--     Operation (ASSIGN (Typed "returnType" INT) (VAL (I 0))),
--     Operation (ASSIGN (Typed "testId" INT) (VAL (I 7)))]
-- ]

checkCall :: Expr -> Expr -> String
checkCall (Callf id args) (Protof id2 args2 _)
    | id /= id2                     = "" 
    | length args /= length args2   = "call: " ++ show id ++ "len arg differ"
    | otherwise                     = ""
-- have to check function argument
-- | _     = checkCallArgument args args2

compareCall :: [Expr] -> Expr -> String
compareCall [] (Callf id _) = "No function named " ++ (show id)
compareCall (p@(Protof (Typed i2 _) _ _ ):rest) c@(Callf (Typed i _) xprs)
    | i == i2       = checkCall c p
    | otherwise     = compareCall rest c

errorWrapper :: String -> [Expr]
errorWrapper "" = []
errorWrapper str = [Err str]

checkErrXp :: [Expr] -> [Expr] -> [Expr]
checkErrXp pts [] = []
checkErrXp pts (x@(Id (Typed str VOID)):xs) = (Err str):(checkErrXp pts xs)
checkErrXp pts (x@(Err _):xs)               = x:(checkErrXp pts xs)
checkErrXp pts ((Exprs e):xs)               = (checkErrXp pts e) ++ (checkErrXp pts xs)
checkErrXp pts ((Protof id1 id2 e):xs)      = (checkErrorId [id1]) ++ (checkErrorId id2) ++ (checkErrXp pts [e]) ++ (checkErrXp pts xs)
checkErrXp pts (e@(Callf _ s):xs)           = res ++ (checkErrXp pts s) ++ (checkErrXp pts xs)
                                        where 
                                        res = errorWrapper (compareCall pts e)
checkErrXp pts ((Unary _ e):xs)             = (checkErrXp pts [e]) ++ (checkErrXp pts xs)
checkErrXp pts ((For (id1, e1) (id2, e2) e3 e4):xs) =  (checkErrorId [id1]) ++ (checkErrXp pts [e1]) ++  (checkErrorId [id2]) ++ (checkErrXp pts [e2]) ++ (checkErrXp pts [e3]) ++ (checkErrXp pts [e4]) ++ (checkErrXp pts xs)
checkErrXp pts ((While e1 e2):xs)           = (checkErrXp pts [e1]) ++ (checkErrXp pts [e2]) ++ (checkErrXp pts xs)
checkErrXp pts ((IfThen e1 e2):xs)          = (checkErrXp pts [e1]) ++ (checkErrXp pts [e2]) ++ (checkErrXp pts xs)
checkErrXp pts ((IfElse e1 e2 e3):xs)       = (checkErrXp pts [e1]) ++ (checkErrXp pts [e2]) ++ (checkErrXp pts [e3]) ++ (checkErrXp pts xs)
checkErrXp pts ((Operation op):xs)          = (checkErrorOp pts[op]) ++ (checkErrXp pts xs)
checkErrXp pts (x:xs) = checkErrXp pts xs

checkErrorId :: [Identifier] -> [Expr]
checkErrorId [] = []
checkErrorId ((Typed str VOID):xs) = (Err str):(checkErrorId xs)
checkErrorId (_:xs) = (checkErrorId xs)

checkErrorOp :: [Expr] -> [Op] -> [Expr]
checkErrorOp pts [] = []
checkErrorOp pts ((XPR e):xs)   = (checkErrXp pts [e]) ++ (checkErrorOp pts xs)
checkErrorOp pts ((ADD op):xs)  = (checkErrorOp pts op) ++ (checkErrorOp pts xs)
checkErrorOp pts ((SUB op):xs)  = (checkErrorOp pts op) ++ (checkErrorOp pts xs)
checkErrorOp pts ((MUL op):xs)  = (checkErrorOp pts op) ++ (checkErrorOp pts xs)
checkErrorOp pts ((DIV op):xs)  = (checkErrorOp pts op) ++ (checkErrorOp pts xs)
checkErrorOp pts ((DataType2.LT op1 op2):xs)    = (checkErrorOp pts [op1]) ++  (checkErrorOp pts [op2]) ++ (checkErrorOp pts xs)
checkErrorOp pts ((DataType2.GT op1 op2):xs)    = (checkErrorOp pts [op1]) ++  (checkErrorOp pts [op2]) ++ (checkErrorOp pts xs)
checkErrorOp pts ((DataType2.EQ op1 op2):xs)    = (checkErrorOp pts [op1]) ++  (checkErrorOp pts [op2]) ++ (checkErrorOp pts xs)
checkErrorOp pts ((DataType2.NOTEQ op1 op2):xs) = (checkErrorOp pts [op1]) ++  (checkErrorOp pts [op2]) ++ (checkErrorOp pts xs)
checkErrorOp pts ((ASSIGN id op):xs) = (checkErrorId [id]) ++ (checkErrorOp pts [op]) ++ (checkErrorOp pts xs)
checkErrorOp pts (x:xs) = checkErrorOp pts xs

getAllProtof :: [Expr] -> [Expr]
getAllProtof []                         = []
getAllProtof ((Protof id args _):rest)  = [(Protof id args (Exprs []))] ++ (getAllProtof rest)
getAllProtof (a:rest)                   = getAllProtof rest

findTrickyError :: [Expr] -> [Expr]
findTrickyError listXps = checkErrXp protos listXps
                        where
                        protos = getAllProtof listXps

-- compareCall :: Expr -> [Expr] -> Bool
-- compareCall c [] = False
-- compareCall c@(Callf (Typed i _) xprs) (p@(Protof (Typed i2 _) _ _ ):rest)
--     | i == i2       = checkCall c p
--     | _             = compareCall c rest

-- -- checkCallArgument :: [Identifier] -> [Expr] -> Bool
-- -- checkCallArgument () () = 

-- checkCall :: Expr -> Expr -> Bool
-- checkCall (Call id args) (Protof id2 args2 _)
--     | id /= id2                     = False 
--     | length args /= length args2   = False
-- -- have to check function argument
-- -- | _     = checkCallArgument args args2


-- -- function to check 
-- checkCallFunction :: [Expr] -> [Expr] -> Maybe ([Expr])
-- checkCallFunction (xpr:xprs) fcts = Just ()


-- checkOpOperator :: [Expr] -> [Op] -> Bool
-- checkOpOperator xprs [((XPR (Callf _ _)):rest)] = bridge xprs <*> checkOperandOperator
-- checkOpOperator xprs [(XPR (Callf _ _))] = cmpCall xprs
-- checkOpOperator xprs [] = cmpCall xprs

-- checkOpCall :: Op -> Bool
-- checkOpCall DIV []          =
-- checkOpCall MUL []          =
-- checkOpCall SUB []          =
-- checkOpCall ADD [a:rest]    = bridge checkOpCall (a)
-- checkOpCall ASSIGN []       =
-- checkOpCall LT _ _          =
-- checkOpCall GT _ _          =
-- checkOpCall NOTEQ _ _       =
-- checkOpCall EQ _ _          =

-- bridge :: a -> b -> Bool
-- bridge a b  | a == True = b
--             | otherwise = False

-- checkStt :: [Expr] -> True
-- checkStt (Operation op)          = checkOpCall op
-- checkStt (IfThen _ (Exprs xprs)) = checkOpCall op
-- checkOpCall Exprs []        =
-- checkOpCall For _ _ _ _     =
-- checkOpCall IfThen _ _      =
-- checkOpCall IfElse _ _ _    =

-- --fct, express list
-- checkCall :: [Expr] -> [Expr] -> Bool
-- checkCall protos ((Protof _ _ (Exprs xprs)):rest)    = 
-- checkCall protos (o@(Operation ()):r)                = True


-- func :: Expr -> Maybe ([Expr])
-- func () = 
