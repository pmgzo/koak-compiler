module TypeInference where
import DataType


-- utils
-------------------------------------------------------------------------------
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


getTypeFromId :: Identifier -> TypeKoak
getTypeFromId (Typed _ t) = t
getTypeFromId _           = VOID -- error

gTFI :: Identifier -> TypeKoak
gTFI id = getTypeFromId id


getTypeFromOp :: [Op] -> TypeKoak
getTypeFromOp ((VAL val):xs)              = gTFV val
getTypeFromOp ((ASSIGN id _):xs)          = gTFI id
getTypeFromOp ((XPR e):xs)                = gTFE [e]
getTypeFromOp ((ADD op):xs)               = getTypeFromOp op
getTypeFromOp ((SUB op):xs)               = getTypeFromOp op
getTypeFromOp ((MUL op):xs)               = getTypeFromOp op
getTypeFromOp ((DIV op):xs)               = getTypeFromOp op
getTypeFromOp ((DataType.LT op _):xs)    = VOID
getTypeFromOp ((DataType.GT op _):xs)    = VOID
getTypeFromOp ((DataType.EQ op _):xs)    = VOID
getTypeFromOp ((DataType.NOTEQ op _):xs) = VOID
getTypeFromOp _                           = VOID

gTFO :: Op -> TypeKoak
gTFO (ADD op) = getTypeFromOp op
gTFO (SUB op) = getTypeFromOp op
gTFO (MUL op) = getTypeFromOp op
gTFO (DIV op) = getTypeFromOp op
gTFO op       = getTypeFromOp [op]

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
getTypeFromExpr ((Operation op):xs)        = gTFO op
getTypeFromExpr ((Exprs e):xs)             = gTFE e
getTypeFromExpr ((IfThen _ e):xs)          = gTFE [e]
getTypeFromExpr ((IfElse _ e _):xs)        = gTFE [e]
getTypeFromExpr _                          = VOID -- error

gTFE :: [Expr] -> TypeKoak
gTFE e                        = getTypeFromExpr e

getIdFromCache :: [Expr] -> Identifier -> Identifier
getIdFromCache [] id = (Typed ("getIdFromCache error"++(show id)) VOID)
getIdFromCache ((Id id1@(Typed name1 t)):xs) id2@(Wait name2)
               | name1 == name2 = id1
               | otherwise      = getIdFromCache xs id2
getIdFromCache _ id@(Typed _ _) = id
getIdFromCache e id = (Typed ("getIdFromCache error"++(show e)++"; "++
    (show id)) VOID)

gIFC :: [Expr] -> Identifier -> Identifier
gIFC e i = getIdFromCache e i




-- replace in expression
-------------------------------------------------------------------------------
checkIdentifier :: [Expr] -> [Expr] -> [Expr]
checkIdentifier [] _ = []
checkIdentifier ((Val val):xs) c = (Val val):(inferType xs c)
checkIdentifier ((Id id):xs) c = (Id (gIFC c id)):(inferType xs c)
checkIdentifier ((Unary u e):xs) c = (Unary u ((inferType [e] c)!!0)):
    (inferType xs c)
checkIdentifier ((Operation op):xs) c = (Operation ((handleOp [op] c)!!0)):
    (inferType xs c)
checkIdentifier ((Exprs e):xs) c = (Exprs (inferType e c)):(inferType xs c)
checkIdentifier ((For (i, val) (i2, cond) inc args):xs) c =
    (handleFor c i val i2 cond inc args):(inferType xs c)
checkIdentifier ((While cond (Exprs args)):xs) c =
    (While ((inferType [cond] c)!!0) (Exprs (inferType args c))):
                                                   (inferType xs c)
checkIdentifier ((IfThen cond (Exprs args)):xs) c =
    (IfThen ((inferType [cond] c)!!0) (Exprs (inferType args c))):
                                                    (inferType xs c)
checkIdentifier ((IfElse cond (Exprs args) (Exprs args2)):xs) c =
    (IfElse ((inferType [cond] c)!!0) (Exprs (inferType args c))
    (Exprs (inferType args2 c))):(inferType xs c)
checkIdentifier _ _ = []

handleOp :: [Op] -> [Expr] -> [Op]
handleOp [] _ = []
handleOp ((ADD ops):xs) c = (ADD (handleOp ops c)):(handleOp xs c)
handleOp ((SUB ops):xs) c = (SUB (handleOp ops c)):(handleOp xs c)
handleOp ((MUL ops):xs) c = (MUL (handleOp ops c)):(handleOp xs c)
handleOp ((DIV ops):xs) c = (DIV (handleOp ops c)):(handleOp xs c)
handleOp ((DataType.LT op1 op2):xs) c =
    (DataType.LT ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):
    (handleOp xs c)
handleOp ((DataType.GT op1 op2):xs) c =
    (DataType.GT ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):
    (handleOp xs c)
handleOp ((DataType.EQ op1 op2):xs) c =
    (DataType.EQ ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):
    (handleOp xs c)
handleOp ((DataType.NOTEQ op1 op2):xs) c =
    (DataType.NOTEQ ((handleOp [op1] c)!!0) ((handleOp [op2] c)!!0)):
    (handleOp xs c)
handleOp (v@(VAL _):xs) c = v:(handleOp xs c)
handleOp (v@(XPR e):xs) c = (XPR ((inferType [e] c)!!0)):(handleOp xs c)
handleOp op c = [(XPR (Err ("error in handleOp "++(show op)++"; "++
    (show c)++"; ")))]

handleIdentifier :: [Expr] -> Identifier -> Op -> [Expr] -> [Expr]
handleIdentifier c id@(Wait name) op@(VAL val) ast = typedExpr:toBeTypedExpr
    where typedExpr     = (Operation (ASSIGN (Typed name (gTFV val)) op))
          toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFV val)))]))
handleIdentifier c id@(Wait n) (XPR (Id id2@(Wait n2))) ast =
    typedExpr:toBeTypedExpr
    where typedExpr = (Operation (ASSIGN (Typed n typ) (XPR (Id (gIFC c id2)))))
          toBeTypedExpr = (inferType ast (c ++ [(Id (Typed n typ))]))
          typ           = gTFC c id2
handleIdentifier c id op ast = [(Err ("error in handleIdentifier "++
    (show c)++"; "++(show id)++"; "++(show op)++"; "++(show ast)))]




-- handle for, while, if
-------------------------------------------------------------------------------
handleFor :: [Expr]->Identifier->Expr->Identifier->Expr->Expr->Expr->Expr
handleFor c i val i2 cond inc (Exprs args) =
    (exprToFor init cT ((inferType [inc] nC)!!0) (Exprs (inferType args nC)))
    where init = (checkAssign [(Operation (ASSIGN i (XPR val)))] c)!!0
          nC   = (assignToCache init):c
          cT   = ((inferType [(Operation op)] nC)!!0)
          op   = DataType.LT (XPR (Id i2)) (XPR cond)
handleFor _ _ _ _ _ _ _ = (Err "error in handleFor")

exprToFor :: Expr -> Expr -> Expr -> Expr -> Expr
exprToFor (Operation (ASSIGN i (XPR val)))
    (Operation (DataType.LT (XPR (Id i2)) (XPR cond))) inc args =
    (For (i, val) (i2, cond) inc args)

assignToCache :: Expr -> Expr
assignToCache (Operation (ASSIGN i val)) = (Id i)






-- assign value
-------------------------------------------------------------------------------
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
    where typedExpr     = (Operation (ASSIGN (Typed name (gTFO newOp)) newOp))
          toBeTypedExpr = (inferType ast (c++[(Id (Typed name (gTFO newOp)))]))
          newOp         = (handleOp [op] c)!!0
handleAssign c id op ast = [(Err ("error in handleAssign "++
    (show c)++"; "++(show id)++"; "++(show op)++"; "++(show ast)))]




-- function
-------------------------------------------------------------------------------
checkFunc :: [Expr] -> [Expr] -> [Expr]
checkFunc ((Callf id args):xs) c = (Callf (gIFC c id) (inferType args c)):
    (inferType xs c)
checkFunc ((Protof id args (Exprs block)):xs) c
          | (alreadyExist id c) == False = handleFunc c id args block xs
          | otherwise = [(Err "checkFunc variable already exist")]
checkFunc _ _ = []

handleFunc :: [Expr] -> Identifier -> [Identifier] -> [Expr] -> [Expr] -> [Expr]
handleFunc c id args block ast = typedExpr:(inferType ast newCache)
    where typedExpr  = (Protof id args (Exprs (inferType block blockCache)))
          newCache   = c ++ [(Id id)]
          blockCache = newCache ++ (identifierToExpr args)







inferType :: [Expr] -> [Expr] -> [Expr]
inferType [] _ = []
inferType exprs c
              | assign /= exprs && assign /= []   = assign
              | func /= []                        = func
              | identifier /= []                  = identifier
              where assign     = checkAssign exprs c
                    func       = checkFunc exprs c
                    identifier = checkIdentifier exprs c
inferType _ _  = []



inferringType :: [Expr] -> [Expr]
inferringType []    = []
inferringType exprs = inferType exprs []
