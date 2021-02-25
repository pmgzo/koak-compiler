module DataType2 where

data TypeKoak = INT | DOUBLE | VOID deriving (Show, Eq)

data Identifier = Wait String | Typed String TypeKoak deriving (Show, Eq)

data Value = Void
           | I Integer
           | D Double deriving (Show, Eq)

data Unop = Not | UMinus deriving (Show, Eq)

-- data Binop = Time | Div | Plus | Minus | Lt | Gt | Eq | NotEq | Assign deriving (Show, Eq)

data Op = VAL Value
        | XPR Expr
        | ADD [Op]
        | SUB [Op]
        | MUL [Op]
        | DIV [Op]
        | LT Op Op
        | GT Op Op
        | EQ Op Op
        | NOTEQ Op Op
        | ASSIGN Identifier Op deriving (Eq, Show)

data Expr = Val Value -- Constant
            | Id Identifier -- Identifier

            | Protof Identifier [Identifier] Expr-- (name, return type) args block

            | Callf Identifier [Expr] -- appel de function avec arguments

            | Exprs [Expr] -- (: y = y + 1: y) list d'expressions qui se suivent

            | Operation Op -- [(Binop, Expr)]
            | Unary Unop Expr -- -5 => UMinus (Val (Int 5))
            | For (Identifier, Expr) (Identifier, Expr) Expr Expr -- exps
            -- for i = 0, i < 1, 1 in print(i);
            -- for i = 0, i < 10, 1 in i = i - 2 * 1 : i = 5 / 5 + i;

            | While Expr Expr --- exp, exps
            | IfThen Expr Expr -- exp, exps
            | IfElse Expr Expr Expr -- exp exps exps
            | Nil
            | Err String deriving (Show, Eq)

-- Operation example:
-- a = 5-- Id (Operation (Assign (Wait "a") (VAL (Int 5))))
-- 5 + 2 + 3 => (Operation (Add [(VAL (Int 5)), (VAL (Int 2)), (VAL (Int 3))]))
-- Operation (Add [(VAL (Int 5)), (Add [(VAL (Int 2)), (VAL (Int 3))]) ]) (ne pas faire ça)

-- IfElse
-- if x then foo() else base()
-- IfElse (Id (Wait "x")) (Call (Wait "foo") []) (Call (Wait "base") [])

-- if a < a then a + 5
-- IfThen (Operation (Lt (Xpr (Id (Wait "a"))) (Xpr (Id (Wait "a")) )))  (Operation (Add [(Xpr (Id (Wait "a"))), (VAL (Int 5))]))