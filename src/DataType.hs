-- Int, Double

-- type Identifier = String

data Identifier = Wait String | Typed String TypeKoak deriving (Show, Eq)

data Unop = Not | UMinus deriving (Show, Eq)

data Binop = Time | Div | Plus | Minus | Lt | Gt | Eq | NotEq | Assign deriving (Show, Eq)

data CallExpr = CallExpr Identifier [Expr] deriving (Show, Eq)

data Lit = D Double | I Int deriving (Show, Eq)

-- simplification call xpr is valid ?
data Postfix = C CallExpr | L Lit | Id Identifier | E Exprs deriving (Show, Eq) -- carefull Grammar Exprs is between parenthesis

-- infinity of unop before postix ?
data Unary = UNOP Unop Unary | P Postfix deriving (Show, Eq)

-- data Expr = U Unary | OP1 Unary [(Binop, Unary)] | OP2 Unary [(Binop, Expr)]

-- 5 + 5 + 5
data Expr = U Unary | OP1 Unary [(Binop, Expr)] deriving (Show, Eq)

-- 6 + 5 : 5 + 7
data Exprs = Exprs [Expr] 
            | For (Identifier, Expr) (Identifier, Expr) Expr Exprs
            | While Expr Exprs
            | IfThen Expr Exprs 
            | IfElse Expr Exprs Exprs deriving (Show, Eq)

data TypeKoak = INT | DOUBLE | VOID deriving (Show, Eq)

type Args = [Identifier]

data Proto = Proto Identifier Args deriving (Show, Eq)-- ++ Unary, Binary

data KDef = Def Proto Expr  | Xpr Exprs deriving (Show, Eq)

type AST = [KDef]
