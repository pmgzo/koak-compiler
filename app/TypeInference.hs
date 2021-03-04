module TypeInference where
import DataType2


-- utils
-------------------------------------------------------------------------------------------
identifierToExpr :: [Identifier] -> [Expr]
identifierToExpr [] = []
identifierToExpr (x:xs) = (Id x):(identifierToExpr xs)

alreadyExist :: Identifier -> [Expr] -> Bool
alreadyExist _ []            = False
alreadyExist id@(Wait name) ((Id (Typed name1 _)):xs)
             | name == name1 = True
             | otherwise     = alreadyExist id xs
alreadyExist id@(Typed name _) ((Id (Typed name1 _)):xs)
             | name == name1 = True
             | otherwise     = alreadyExist id xs


getTypeFromValue :: Value -> TypeKoak
getTypeFromValue (I nb) = INT
getTypeFromValue (D nb) = DOUBLE
getTypeFromValue Void   = VOID
-- getTypeFromValue _      = VOID

gTFV :: Value -> TypeKoak
gTFV val = getTypeFromValue val

getTypeFromOp :: [Op] -> [Expr] -> TypeKoak
getTypeFromOp ((VAL val):xs) _             = gTFV val
getTypeFromOp ((XPR e):xs) c               = gTFE [e]
getTypeFromOp ((ADD op):xs) c              = getTypeFromOp op c
getTypeFromOp ((SUB op):xs) c              = getTypeFromOp op c
getTypeFromOp ((MUL op):xs) c              = getTypeFromOp op c
getTypeFromOp ((DIV op):xs) c              = getTypeFromOp op c
getTypeFromOp ((DataType2.LT _ _):xs) c    = VOID
getTypeFromOp ((DataType2.GT _ _):xs) c    = VOID
getTypeFromOp ((DataType2.EQ _ _):xs) c    = VOID
getTypeFromOp ((DataType2.NOTEQ _ _):xs) c = VOID
getTypeFromOp _ _ = VOID

gTFO :: Op -> [Expr] -> TypeKoak
gTFO (ADD op) c = getTypeFromOp op c
gTFO (SUB op) c = getTypeFromOp op c
gTFO (MUL op) c = getTypeFromOp op c
gTFO (DIV op) c = getTypeFromOp op c
gTFO op c = getTypeFromOp [op] c

getTypeFromCache :: [Expr] -> Identifier -> TypeKoak
getTypeFromCache [] _ = VOID -- error
getTypeFromCache ((Id id1@(Typed name1 t)):xs) id2@(Wait name2)
                  | name1 == name2 = t
                  | otherwise      = getTypeFromCache xs id2

gTFC :: [Expr] -> Identifier -> TypeKoak
gTFC e i = getTypeFromCache e i

getTypeFromExpr :: [Expr] -> TypeKoak
getTypeFromExpr []                         = VOID -- error
getTypeFromExpr ((Id (Typed _ t)):xs)      = t
getTypeFromExpr ((Unary Not e):xs)         = VOID
getTypeFromExpr ((Unary _ e):xs)           = gTFE [e]
getTypeFromExpr ((Val val):xs)             = gTFV val
getTypeFromExpr ((Callf (Typed _ t) _):xs) = t
getTypeFromExpr ((Operation (XPR e)):xs)   = gTFE [e]
getTypeFromExpr ((Operation op):xs)        = gTFO op []

gTFE :: [Expr] -> TypeKoak
gTFE e                        = getTypeFromExpr e

getIdFromCache :: [Expr] -> Identifier -> Identifier
getIdFromCache [] id = (Typed ("getIdFromCache error"++(show id)) VOID)
getIdFromCache ((Id id1@(Typed name1 t)):xs) id2@(Wait name2)
                  | name1 == name2 = id1
                  | otherwise      = getIdFromCache xs id2
getIdFromCache _ id@(Typed _ _) = id
-- getIdFromCache e id = (Typed ("getIdFromCache error"++(show e)++"; "++(show id)) VOID)

gIFC :: [Expr] -> Identifier -> Identifier
gIFC e i = getIdFromCache e i




-- replace in expression
-------------------------------------------------------------------------------------------
checkIdentifier :: [Expr] -> [Expr] -> [Expr]
checkIdentifier [] _ = []
checkIdentifier ((Val val):xs) c = (Val val):(inferType xs c)
checkIdentifier ((Id id):xs) c = (Id (gIFC c id)):(inferType xs c)
checkIdentifier ((Unary u e):xs) c = (Unary u ((inferType [e] c)!!0)):(inferType xs c)
checkIdentifier ((Operation op):xs) c = (Operation ((handleOp [op] c)!!0)):(inferType xs c)
checkIdentifier ((Exprs e):xs) c = (Exprs (inferType e c)):(inferType xs c)
checkIdentifier ((For (i, val) (i2, cond) inc args):xs) c = (handleFor c i val i2 cond inc args):(inferType xs c)
checkIdentifier ((While cond (Exprs args)):xs) c = (While ((inferType [cond] c)!!0) (Exprs (inferType args c))):(inferType xs c)
checkIdentifier ((IfThen cond (Exprs args)):xs) c = (IfThen ((inferType [cond] c)!!0) (Exprs (inferType args c))):(inferType xs c)
checkIdentifier ((IfElse cond (Exprs args) (Exprs args2)):xs) c = (IfElse ((inferType [cond] c)!!0) (Exprs (inferType args c)) (Exprs (inferType args2 c))):(inferType xs c)
checkIdentifier _ _ = []

handleOp :: [Op] -> [Expr] -> [Op]
handleOp [] _ = []
handleOp ((ADD ops):xs) c = (ADD (handleOp ops c)):(handleOp xs c)
handleOp ((SUB ops):xs) c = (SUB (handleOp ops c)):(handleOp xs c)
handleOp ((MUL ops):xs) c = (MUL (handleOp ops c)):(handleOp xs c)
handleOp ((DIV ops):xs) c = (DIV (handleOp ops c)):(handleOp xs c)
handleOp ((DataType2.LT op1 op2):xs) c = (DataType2.LT ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):(handleOp xs c)
handleOp ((DataType2.GT op1 op2):xs) c = (DataType2.GT ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):(handleOp xs c)
handleOp ((DataType2.EQ op1 op2):xs) c = (DataType2.EQ ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):(handleOp xs c)
handleOp ((DataType2.NOTEQ op1 op2):xs) c = (DataType2.NOTEQ ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):(handleOp xs c)
handleOp (v@(VAL _):xs) c = v:(handleOp xs c)
-- handleOp (v@(XPR (Id id)):xs) c = (XPR (Id (gIFC c id))):(handleOp xs c)
handleOp (v@(XPR e):xs) c = (XPR ((inferType [e] c)!!0)):(handleOp xs c)
handleOp op c = [(XPR (Err ("error in handleOp "++(show op)++"; "++(show c)++"; ")))]

handleIdentifier :: [Expr] -> Identifier -> Op -> [Expr] -> [Expr]
handleIdentifier c id@(Wait name) op@(VAL val) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name (gTFV val)) op))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFV val)))]))
handleIdentifier c id@(Wait name) (XPR (Id id2@(Wait n))) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name typ) (XPR (Id (gIFC c id2)))))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name typ))]))
                   typ           = gTFC c id2
handleIdentifier c id op ast = [(Err ("error in handleIdentifier "++(show c)++"; "++(show id)++"; "++(show op)++"; "++(show ast)))]




-- handle for, while, if
-------------------------------------------------------------------------------------------
handleFor :: [Expr] -> Identifier -> Expr -> Identifier -> Expr -> Expr -> Expr -> Expr
handleFor c i val i2 cond inc (Exprs args)
          = (exprToFor init condT ((inferType [inc] newC)!!0) (Exprs (inferType args newC)))
             where init          = (checkAssign [(Operation (ASSIGN i (XPR val)))] c)!!0
                   newC          = (assignToCache init):c
                   condT         = ((inferType [(Operation (DataType2.LT (XPR (Id i2)) (XPR cond)))] newC)!!0)
handleFor _ _ _ _ _ _ _ = (Err "error in handleFor")

exprToFor :: Expr -> Expr -> Expr -> Expr -> Expr
exprToFor (Operation (ASSIGN i (XPR val))) (Operation (DataType2.LT (XPR (Id i2)) (XPR cond))) inc args = (For (i, val) (i2, cond) inc args)

assignToCache :: Expr -> Expr
assignToCache (Operation (ASSIGN i val)) = (Id i)






-- assign value -- to add assign Unary, Operation, Callf
-------------------------------------------------------------------------------------------
checkAssign :: [Expr] -> [Expr] -> [Expr]
checkAssign ((Operation (ASSIGN id op)):xs) c = handleAssign c id op xs
checkAssign _ _ = []

handleAssign :: [Expr] -> Identifier -> Op -> [Expr] -> [Expr]
handleAssign c id@(Wait name) op@(VAL val) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name (gTFV val)) op))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFV val)))]))
handleAssign c id@(Wait name) (XPR expr) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name typ) (XPR (e))))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name typ))]))
                   e             = (inferType [expr] c)!!0
                   typ           = gTFE [e]
handleAssign c id@(Wait name) op ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name (gTFO newOp c)) newOp))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFO newOp c)))]))
                   newOp         = (handleOp [op] c)!!0
handleAssign c id op ast = [(Err ("error in handleAssign "++(show c)++"; "++(show id)++"; "++(show op)++"; "++(show ast)))]




-- function -- to add assign Unary, Operation, Callf
-------------------------------------------------------------------------------------------
checkFunc :: [Expr] -> [Expr] -> [Expr]
checkFunc ((Callf id args):xs) c = (Callf (gIFC c id) (inferType args c)):(inferType xs c)
checkFunc ((Protof id args (Exprs block)):xs) c
          | (alreadyExist id c) == False = handleFunc c id args block xs
          | otherwise = [(Err "checkFunc variable already exist")]
checkFunc _ _ = []

handleFunc :: [Expr] -> Identifier -> [Identifier] -> [Expr] -> [Expr] -> [Expr]
handleFunc c id args block ast = typedExpr:(inferType ast newCache)
             where typedExpr  = (Protof id args (Exprs (inferType block blockCache)))
                   newCache   = c ++ [(Id id)]
                   blockCache = newCache ++ (identifierToExpr args)
-- handleFunc c id ag b ast = [(Err ("error in handleFunc"++(show c)++"; "++(show id)++"; "++(show ag)++"; "++(show b)++"; "++(show ast)))]

handleFuncArgs :: [Expr] -> [Expr] -> [Expr] -> [Expr]
handleFuncArgs [] _ _ = [] -- error
handleFuncArgs _ _ [] = [] -- end args (normal)
handleFuncArgs c (y@(Id (Typed name1 t)):ys) args@((Id (Wait name2)):xs)
               | name1 == name2 = y:(handleFuncArgs c c xs) -- add check if type arg expected match
               | otherwise      = handleFuncArgs c ys args
handleFuncArgs c tmpCache (x@(Id (Typed _ _)):xs) = x:(handleFuncArgs c c xs) -- add check if type arg expected match
handleFuncArgs c tmpCache (x@(Val _):xs) = x:(handleFuncArgs c c xs) -- add check if type arg expected match







inferType :: [Expr] -> [Expr] -> [Expr]
-- inferType [] c = c -- TO GET CACHE TO DEBUG
inferType [] _ = []
inferType exprs c
              | assign /= exprs && assign /= []   = assign
              | func /= []                        = func
              | identifier /= []                  = identifier
              where assign     = checkAssign exprs c
                    func       = checkFunc exprs c
                    identifier = checkIdentifier exprs c
inferType _ _  = []












-- Func to call to handle type inference
inferringType :: [Expr] -> [Expr]
inferringType [] = []
inferringType exprs
              | error == [] = inferredType
              | otherwise   = error
              where inferredType = inferType exprs []
                    error        = checkError inferredType




-- keep only (Err str) in the list
checkError :: [Expr] -> [Expr]
checkError [] = []
checkError (x@(Id (Typed str VOID)):xs) = (Err str):(checkError xs)
checkError (x@(Err _):xs)               = x:(checkError xs)
checkError ((Exprs e):xs)               = (checkError e) ++ (checkError xs)
checkError ((Protof id1 id2 e):xs)      = (checkErrorId [id1]) ++ (checkErrorId id2) ++ (checkError [e]) ++ (checkError xs)
checkError ((Callf id e):xs)            = (checkErrorId [id]) ++ (checkError e) ++ (checkError xs)
checkError ((Unary _ e):xs)             = (checkError [e]) ++ (checkError xs)
checkError ((For (id1, e1) (id2, e2) e3 e4):xs) =  (checkErrorId [id1]) ++ (checkError [e1]) ++  (checkErrorId [id2]) ++ (checkError [e2]) ++ (checkError [e3]) ++ (checkError [e4]) ++ (checkError xs)
checkError ((While e1 e2):xs)           = (checkError [e1]) ++ (checkError [e2]) ++ (checkError xs)
checkError ((IfThen e1 e2):xs)          = (checkError [e1]) ++ (checkError [e2]) ++ (checkError xs)
checkError ((IfElse e1 e2 e3):xs)       = (checkError [e1]) ++ (checkError [e2]) ++ (checkError [e3]) ++ (checkError xs)
checkError ((Operation op):xs)          = (checkErrorOp [op]) ++ (checkError xs)
checkError (x:xs) = checkError xs

checkErrorId :: [Identifier] -> [Expr]
checkErrorId [] = []
checkErrorId ((Typed str VOID):xs) = (Err str):(checkErrorId xs)
checkErrorId (_:xs) = (checkErrorId xs)

checkErrorOp :: [Op] -> [Expr]
checkErrorOp [] = []
checkErrorOp ((XPR e):xs)   = (checkError [e]) ++ (checkErrorOp xs)
checkErrorOp ((ADD op):xs)  = (checkErrorOp op) ++ (checkErrorOp xs)
checkErrorOp ((SUB op):xs)  = (checkErrorOp op) ++ (checkErrorOp xs)
checkErrorOp ((MUL op):xs)  = (checkErrorOp op) ++ (checkErrorOp xs)
checkErrorOp ((DIV op):xs)  = (checkErrorOp op) ++ (checkErrorOp xs)
checkErrorOp ((DataType2.LT op1 op2):xs)    = (checkErrorOp [op1]) ++  (checkErrorOp [op2]) ++ (checkErrorOp xs)
checkErrorOp ((DataType2.GT op1 op2):xs)    = (checkErrorOp [op1]) ++  (checkErrorOp [op2]) ++ (checkErrorOp xs)
checkErrorOp ((DataType2.EQ op1 op2):xs)    = (checkErrorOp [op1]) ++  (checkErrorOp [op2]) ++ (checkErrorOp xs)
checkErrorOp ((DataType2.NOTEQ op1 op2):xs) = (checkErrorOp [op1]) ++  (checkErrorOp [op2]) ++ (checkErrorOp xs)
checkErrorOp ((ASSIGN id op):xs) = (checkErrorId [id]) ++ (checkErrorOp [op]) ++ (checkErrorOp xs)
checkErrorOp (x:xs) = checkErrorOp xs
