module Error where

import CheckBool
import DataType2
import TypeInference

compareArg :: Expr -> Identifier -> Bool
compareArg g (Typed _ t)    = (t == t2)
                            where t2 = gTFE [g]

compareCallArgs :: [Expr] -> [Identifier] -> Bool
compareCallArgs [] []           = True
compareCallArgs (arg:rest) (id:rest2)
                | res == False  = False
                | otherwise     = compareCallArgs rest rest2
                where res = compareArg arg id

checkCall :: Expr -> Expr -> String
checkCall (Callf id args) (Protof id2 args2 _)
    | length args /= length args2   = "call: " ++ show id ++ "len arg differ"
    | typeArgsAreOkay == False      = "wrong type args in call " ++ show id
    | otherwise                     = ""
    where
    typeArgsAreOkay = compareCallArgs args args2

compareCall :: [Expr] -> Expr -> String
compareCall [] (Callf id _) = "No function named " ++ (show id)
compareCall (p@(Protof (Typed i2 _) _ _ ):rest) c@(Callf (Typed i _) xprs)
    | i == i2       = checkCall c p
    | otherwise     = compareCall rest c

getTypeFromOpError :: [Op] -> TypeKoak
getTypeFromOpError ((VAL val):xs)              = gTFV val
getTypeFromOpError ((ASSIGN id _):xs)          = gTFI id
getTypeFromOpError ((XPR e):xs)                = gTFEE [e]
getTypeFromOpError ((ADD op):xs)               = getTypeFromOpError op
getTypeFromOpError ((SUB op):xs)               = getTypeFromOpError op
getTypeFromOpError ((MUL op):xs)               = getTypeFromOpError op
getTypeFromOpError ((DIV op):xs)               = getTypeFromOpError op
getTypeFromOpError ((DataType2.LT _ _):xs)     = BOOL
getTypeFromOpError ((DataType2.GT _ _):xs)     = BOOL
getTypeFromOpError ((DataType2.EQ _ _):xs)     = BOOL
getTypeFromOpError ((DataType2.NOTEQ op _):xs) = BOOL
getTypeFromOpError _                           = VOID

gTFOE :: Op -> TypeKoak
gTFOE (ADD op) = getTypeFromOpError op
gTFOE (SUB op) = getTypeFromOpError op
gTFOE (MUL op) = getTypeFromOpError op
gTFOE (DIV op) = getTypeFromOpError op
gTFOE op       = getTypeFromOpError [op]

getTypeFromExprError :: [Expr] -> TypeKoak
getTypeFromExprError []                         = VOID -- error
getTypeFromExprError ((Id (Typed _ t)):xs)      = t
getTypeFromExprError ((Unary u e):xs)           = gTFEE [e]
getTypeFromExprError ((Val val):xs)             = gTFV val
getTypeFromExprError ((Callf (Typed _ t) _):xs) = t
getTypeFromExprError ((Operation op):xs)        = gTFOE op
getTypeFromExprError ((Exprs e):xs)             = gTFEE e
getTypeFromExprError ((IfThen _ e):xs)          = gTFEE [e]
getTypeFromExprError ((IfElse _ e _):xs)        = gTFEE [e]
getTypeFromExprError _                          = VOID -- error

gTFEE :: [Expr] -> TypeKoak
gTFEE e = getTypeFromExprError e


checkTypeOfFunction :: [Expr] -> Identifier -> [Expr]
checkTypeOfFunction [] _                    = []
checkTypeOfFunction ((Exprs e):[]) id       = checkTypeOfFunction e id
checkTypeOfFunction ((IfThen _ e):[]) id    = checkTypeOfFunction [e] id
checkTypeOfFunction ((IfElse _ e _):[]) id  = checkTypeOfFunction [e] id
checkTypeOfFunction (x:[]) id
                    | gTFEE [x] == gTFI id  = []
                    | otherwise = [(Err (show id ++ " expected but got " ++
                                        (show $ gTFEE [x]) ++ " (" ++ show x ++ ")"))]
checkTypeOfFunction (x:xs) id               = checkTypeOfFunction xs id



errorWrapper :: String -> [Expr]
errorWrapper "" = []
errorWrapper str = [Err str]

checkErrXp :: [Expr] -> [Expr] -> TypeKoak -> [Expr]
checkErrXp _ [] t = []
checkErrXp pts ((Id id):xs) t                 = (checkErrorId [id] t) ++
    (checkErrXp pts xs t)
checkErrXp pts (x@(Err _):xs) t               = x:(checkErrXp pts xs t)
checkErrXp pts ((Exprs e):xs) t               = (checkErrXp pts e t) ++
    (checkErrXp pts xs t)
checkErrXp pts ((Protof id1 id2 e):xs) t      = (checkTypeOfFunction [e] id1) ++
    (checkErrorId [id1] t) ++ (checkErrorId id2 t) ++ (checkErrXp pts [e] t) ++
    (checkErrXp pts xs t)
checkErrXp pts (e@(Callf _ s):xs) t           = res ++ (checkErrXp pts s t) ++
    (checkErrXp pts xs t)
    where res = errorWrapper (compareCall pts e)
checkErrXp pts ((Unary _ e):xs) t             = (checkErrXp pts [e] t) ++
    (checkErrXp pts xs t)
checkErrXp pts ((For (id1, e1) (id2, e2) e3 e4):xs) t = (checkErrorId [id1] t) ++
    (checkErrXp pts [e1] t) ++ (checkErrorId [id2] t) ++
    (checkErrXp pts [e2] t) ++ (checkErrXp pts [e3] t) ++
    (checkErrXp pts [e4] t) ++ (checkErrXp pts xs t)
checkErrXp pts ((While e1 e2):xs) t           = (checkErrXp pts [e1] t) ++
    (checkErrXp pts [e2] t) ++ (checkErrXp pts xs t)
checkErrXp pts ((IfThen e1 e2):xs) t          = (checkErrXp pts [e1] t) ++
    (checkErrXp pts [e2] t) ++ (checkErrXp pts xs t)
checkErrXp pts ((IfElse e1 e2 e3):xs) t       = (checkErrXp pts [e1] t) ++
    (checkErrXp pts [e2] t) ++ (checkErrXp pts [e3] t) ++ (checkErrXp pts xs t)
checkErrXp pts ((Operation op):xs) t          = (checkErrorOp pts [op] (gTFOE op)) ++
    (checkErrXp pts xs t)
checkErrXp pts (x@(Val v):xs) t
             | t /= NULL && gTFV v /= t = (Err (show t++" expected but got "++
    (show $ gTFV v)++" ("++show x++")")):(checkErrXp pts xs t)
             | otherwise   = checkErrXp pts xs t
checkErrXp pts (x:xs) t = checkErrXp pts xs t

checkErrorId :: [Identifier] -> TypeKoak -> [Expr]
checkErrorId [] _                    = []
checkErrorId ((Typed str VOID):xs) t = (Err str):(checkErrorId xs t)
checkErrorId (x@(Typed str t1):xs) t
             | t /= NULL && t1 /= t = (Err (show t ++ " expected but got " ++
    show x)):(checkErrorId xs t)
             | otherwise            = checkErrorId xs t
checkErrorId (_:xs) t = (checkErrorId xs t)

checkErrorOp :: [Expr] -> [Op] -> TypeKoak -> [Expr]
checkErrorOp _ [] _ = []
checkErrorOp pts ((XPR e):xs) t   = (checkErrXp pts [e] t) ++
    (checkErrorOp pts xs t)
checkErrorOp pts ((ADD op):xs) t  = (checkErrorOp pts op t) ++
    (checkErrorOp pts xs t)
checkErrorOp pts ((SUB op):xs) t  = (checkErrorOp pts op t) ++
    (checkErrorOp pts xs t)
checkErrorOp pts ((MUL op):xs) t  = (checkErrorOp pts op t) ++
    (checkErrorOp pts xs t)
checkErrorOp pts ((DIV op):xs) t  = (checkErrorOp pts op t) ++
    (checkErrorOp pts xs t)
checkErrorOp pts ((DataType2.LT op1 op2):xs) t    =
    (checkErrorOpCond (gTFOE op1) (gTFOE op2)) ++
    (checkErrorOp pts [op1] (gTFOE op1)) ++
    (checkErrorOp pts [op2] (gTFOE op1)) ++ (checkErrorOp pts xs t)
checkErrorOp pts ((DataType2.GT op1 op2):xs) t    =
    (checkErrorOpCond (gTFOE op1) (gTFOE op2)) ++
    (checkErrorOp pts [op1] (gTFOE op1)) ++
    (checkErrorOp pts [op2] (gTFOE op1)) ++ (checkErrorOp pts xs t)
checkErrorOp pts ((DataType2.EQ op1 op2):xs) t    =
    (checkErrorOpCond (gTFOE op1) (gTFOE op2)) ++
    (checkErrorOp pts [op1] (gTFOE op1)) ++
    (checkErrorOp pts [op2] (gTFOE op1)) ++ (checkErrorOp pts xs t)
checkErrorOp pts ((DataType2.NOTEQ op1 op2):xs) t =
    (checkErrorOpCond (gTFOE op1) (gTFOE op2)) ++
    (checkErrorOp pts [op1] (gTFOE op1)) ++
    (checkErrorOp pts [op2] (gTFOE op1)) ++ (checkErrorOp pts xs t)
checkErrorOp pts ((ASSIGN id op):xs) t            = (checkErrorId [id] t) ++
    (checkErrorOp pts [op] t) ++ (checkErrorOp pts xs t)
checkErrorOp pts (x@(VAL v):xs) t
             | t /= NULL && gTFV v /= t = (Err (show t++" expected but got "++
    (show $ gTFV v)++" ("++show x++")")):(checkErrorOp pts xs t)
             | otherwise   = checkErrorOp pts xs t
checkErrorOp pts (x:xs) t = checkErrorOp pts xs t



checkErrorOpCond :: TypeKoak -> TypeKoak -> [Expr]
checkErrorOpCond t1 t2
             | t1 /= t2     = [Err (show t1++" expected but got "++(show t2))]
             | otherwise   = []

getAllProtof :: [Expr] -> [Expr]
getAllProtof []                         = []
getAllProtof ((Protof id args _):rest)  = [(Protof id args (Exprs []))] ++
    (getAllProtof rest)
getAllProtof (a:rest)                   = getAllProtof rest

findTrickyError :: [Expr] -> [Expr]
findTrickyError listXps = checkErrXp protos listXps NULL ++
    checkBinOperation listXps
    where protos = getAllProtof listXps
