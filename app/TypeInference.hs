module TypeInference where
import DataType2


-- -- def coco(double: x) double: x;

-- -- Paul
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Wait "x"))) []))
-- -- Aurele
-- (Def (Proto (Typed "coco" DOUBLE) [(Typed "x" DOUBLE)]) (OP1  (P (Id (Typed "x" DOUBLE))) []))

-- -- y = 2.0;

-- (Operation (ASSIGN (Wait "y") (VAL (I 2))))

-- (Xpr (OP1 (P (Id (Typed "y" DOUBLE))) [(Assign, (OP1 (P (L (D 2.0 ))) [] ) ) ] ) )

-- -- while y < 10 do y = y * 2;

-- (Xpr (While (OP1 (P (Id (Wait "y") ) )  [(Lt, (U (P (L (I 10))) ))] ) (Exprs [ (OP1 (P (Id (Wait "y" ))) [(Assign, (OP1 (P (Id (Wait "y"))) [(Time, (U (P (L (I 2))) ) )]) )] )] )) )

-- (Xpr (While (OP1 (P (Id (Typed "y" DOUBLE) ) )  [(Lt, (U (P (L (I 10))) ))] ) (Exprs [ (OP1 (P (Id (Typed "y" DOUBLE))) [(Assign, (OP1 (P (Id (Typed "y" DOUBLE))) [(Time, (U (P (L (I 2))) ) )]) )] )] )) )


-- utils
-------------------------------------------------------------------------------------------

identifierToExpr :: [Identifier] -> [Expr]
identifierToExpr [] = []
identifierToExpr (x:xs) = (Id x):(identifierToExpr xs)




getTypeFromValue :: Value -> TypeKoak
getTypeFromValue (I nb) = INT
getTypeFromValue (D nb) = DOUBLE
getTypeFromValue Void   = VOID
-- getTypeFromValue _      = VOID

gTFV :: Value -> TypeKoak
gTFV val = getTypeFromValue val

getTypeFromCache :: [Expr] -> Identifier -> TypeKoak
getTypeFromCache [] _ = VOID -- error
getTypeFromCache ((Id id1@(Typed name1 t)):xs) id2@(Wait name2)
                  | name1 == name2 = t
                  | otherwise      = getTypeFromCache xs id2

gTFC :: [Expr] -> Identifier -> TypeKoak
gTFC e i = getTypeFromCache e i

getIdFromCache :: [Expr] -> Identifier -> Identifier
getIdFromCache [] _ = (Typed "getIdFromCache error" VOID) -- error
getIdFromCache ((Id id1@(Typed name1 t)):xs) id2@(Wait name2)
                  | name1 == name2 = id1
                  | otherwise      = getIdFromCache xs id2

gIFC :: [Expr] -> Identifier -> Identifier
gIFC e i = getIdFromCache e i
-------------------------------------------------------------------------------------------




-- replace in expression
-------------------------------------------------------------------------------------------
checkIdentifier :: [Expr] -> [Expr] -> [Expr]
checkIdentifier [] _ = []
checkIdentifier ((Operation op):xs) c = (Operation ((handleOp [op] c)!!0)):(checkIdentifier xs c)
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
handleOp (v@(XPR e):xs) c = (XPR ((checkIdentifier [e] c)!!0)):(handleOp xs c)
handleOp _ _ = []

handleIdentifier :: [Expr] -> Identifier -> Op -> [Expr] -> [Expr]
handleIdentifier c id@(Wait name) op@(VAL val) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name (gTFV val)) op))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFV val)))]))
handleIdentifier c id@(Wait name) (XPR (Id id2@(Wait n))) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name typ) (XPR (Id (gIFC c id2)))))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name typ))]))
                   typ           = gTFC c id2
handleIdentifier _ _ _ _ = []






-- assign value -- to add assign Unary, Operation, Callf
-------------------------------------------------------------------------------------------
checkAssign :: [Expr] -> [Expr] -> [Expr]
checkAssign ((Operation (ASSIGN id op)):xs) c = handleAssign c id op xs
checkAssign _ _ = []

handleAssign :: [Expr] -> Identifier -> Op -> [Expr] -> [Expr]
handleAssign c id@(Wait name) op@(VAL val) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name (gTFV val)) op))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name (gTFV val)))]))
handleAssign c id@(Wait name) (XPR (Id id2@(Wait n))) ast = typedExpr:toBeTypedExpr
             where typedExpr     = (Operation (ASSIGN (Typed name typ) (XPR (Id (gIFC c id2)))))
                   toBeTypedExpr = (inferType ast (c ++ [(Id (Typed name typ))]))
                   typ           = gTFC c id2
handleAssign _ _ _ _ = []




-- function -- to add assign Unary, Operation, Callf
-------------------------------------------------------------------------------------------
checkFunc :: [Expr] -> [Expr] -> [Expr]
checkFunc ((Protof id args (Exprs block)):xs) c = handleFunc c id args block xs
checkFunc ((Callf id args):xs) c = (Callf (gIFC c id) (handleFuncArgs c c args)):(inferType xs c)
checkFunc _ _ = []

handleFunc :: [Expr] -> Identifier -> [Identifier] -> [Expr] -> [Expr] -> [Expr]
handleFunc c id args block ast = typedExpr:(inferType ast newCache)
             where typedExpr  = (Protof id args (Exprs (inferType block blockCache)))
                   newCache   = c ++ [(Id id)]
                   blockCache = newCache ++ (identifierToExpr args)
handleFunc _ _ _ _ _ = []

handleFuncArgs :: [Expr] -> [Expr] -> [Expr] -> [Expr]
handleFuncArgs [] _ _ = [] -- error
handleFuncArgs _ _ [] = [] -- end args (normal)
handleFuncArgs c (y@(Id (Typed name1 t)):ys) args@((Id (Wait name2)):xs)
               | name1 == name2 = y:(handleFuncArgs c c xs) -- add check if type arg expected match
               | otherwise      = handleFuncArgs c ys args
handleFuncArgs c tmpCache (x@(Id (Typed _ _)):xs) = x:(handleFuncArgs c c xs) -- add check if type arg expected match
handleFuncArgs c tmpCache (x@(Val _):xs) = x:(handleFuncArgs c c xs) -- add check if type arg expected match




---- handle in / out of block for assignement
---- check if every situation is handled, assignement in if, for, while, a=b=c, ...
---- change to maybe ([Expr]) to handle error
---- 2 cache : cFunc, cVar




inferType :: [Expr] -> [Expr] -> [Expr]
-- inferType [] c = c -- TO GET CACHE TO DEBUG
inferType [] _ = []
inferType exprs c
              | assign /= exprs && assign /= []         = assign
              | func /= exprs && func /= []             = func
              | identifier /= exprs && identifier /= [] = identifier
              where assign     = checkAssign exprs c
                    func       = checkFunc exprs c
                    identifier = checkIdentifier exprs c
inferType _ _  = []

inferringType :: [Expr] -> [Expr] -> [Expr]
inferringType [] _ = []
inferringType exprs c = inferType exprs c


------------------------------------------------------------------
changeType :: Identifier -> TypeKoak -> Identifier
changeType (Wait name) typeId = (Typed name typeId)
changeType id _               = id
------------------------------------------------------------------


-- checkAssign [(Operation (ASSIGN (Wait "y") (VAL (I 5))))] []
-- inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y")))))] []



            -- | Protof Identifier [Identifier] Expr -- (name, return type) args block
            -- | Callf Identifier [Expr] -- appel de function avec arguments

-- inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y"))))), (Protof (Typed "add" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))]))] []
-- inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y"))))), (Protof (Typed "add" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))])),
-- inferringType [(Operation (ASSIGN (Wait "y") (VAL (I 5)))), (Operation (ASSIGN (Wait "x") (XPR (Id (Wait "y"))))), (Protof (Typed "add" INT) [(Typed "a" INT), (Typed "b" INT)] (Exprs [(Operation (ADD [(XPR (Id (Wait "a"))), (XPR (Id (Wait "a")))]))])), (Callf (Wait "add") [(Val (I 2)), (Id (Wait "x"))])] []
