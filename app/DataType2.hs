module DataType2 where

data TypeKoak = INT | DOUBLE | VOID | BOOL | NULL deriving (Show, Eq)

data Identifier = Wait String | Typed String TypeKoak deriving (Show, Eq)

data Value = Void
           | I Integer
           | D Double deriving (Show, Eq)

data Unop = Not | UMinus deriving (Show, Eq)

data Op = VAL Value
        | XPR Expr
        | PAR Op
        | ADD [Op]
        | SUB [Op]
        | MUL [Op]
        | DIV [Op]
        | LT Op Op
        | GT Op Op
        | EQ Op Op
        | NOTEQ Op Op
        | ASSIGN Identifier Op deriving (Eq, Show)

data Expr = Val Value
            | Id Identifier

            | Protof Identifier [Identifier] Expr
            | Callf Identifier [Expr]

            | Exprs [Expr]

            | Operation Op
            | Unary Unop Expr
            | For (Identifier, Expr) (Identifier, Expr) Expr Expr
            | While Expr Expr
            | IfThen Expr Expr
            | IfElse Expr Expr Expr
            | Nil
            | Err String deriving (Show, Eq)
