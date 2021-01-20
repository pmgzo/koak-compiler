
data TypeKoak = INT | DOUBLE | VOID deriving (Show, Eq)

data Identifier = Wait String | Typed String TypeKoak deriving (Show, Eq) 

data Value
    = Void
    | Int Integer
    | Double Double deriving (Show, Eq) 

data Unop = Not | UMinus deriving (Show, Eq)

-- data Binop = Time | Div | Plus | Minus | Lt | Gt | Eq | NotEq | Assign deriving (Show, Eq)

data Op = VAL Value
        | Xpr Expr
        | Add [Op]
        | Sub [Op]
        | Mul [Op]
        | Div [Op]
        | Lt Op Op
        | Gt Op Op
        | Eq Op Op
        | NotEq Op Op
        | Assign Identifier Op deriving (Eq, Show)

data Expr = Val Value --Constant
            | Id Identifier -- Identifier
            
            | Protof Identifier [Identifier] Expr -- (name, return type ) args block

            | Call Identifier [Expr] -- appel de function avec arguments

            | Exprs [Expr] -- (: y = y + 1: y) list d'expressions qui se suivent
            
            
            | Operation Op -- [(Binop, Expr)]
            | Unary Unop Expr -- -5 => UMinus (Val (Int 5))
            | For (Identifier, Expr) (Identifier, Expr) Expr Expr
            -- for i = 0, i < 1, 1 in print(i);
            -- for i = 0, i < 10, 1 in i = i - 2 * 1 : i = 5 / 5 + i;
            | While Expr Expr --- exp, exps
            | IfThen Expr Expr -- exp, exps
            | IfElse Expr Expr Expr deriving (Show, Eq) -- exp exps exps

-- Operation example:
-- a = 5-- Id (Wait "a") [(Binop Assign) (Val Int 5)] 
-- 5 + 2 + 3 => Val (Int 5) [(Binop Plus) (Val 2), (Binop Plus) (Val 3)] 

-- IfElse
-- if x then foo() else base()
-- IfElse (Id (Wait "x")) (Call (Wait "foo") []) (Call (Wait "base") [])

-- if a < a then a + 5
-- IfThen (Operation (Id (Wait "a")) [Lt (Id (Wait "a"))]) (Operation (Id (Wait "a")) [(Binop Plus) (Val (Int 5))])

-- IfThen (Operation [Lt (Xpr Id Wait "a") (Xpr Id Wait "a")] )
