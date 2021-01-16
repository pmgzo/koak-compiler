type Identifier = String
-- Int, Double


data Unop = Not | UMinus
data Binop = Time | Div | Plus | Minus | Lt | Gt | Eq | NotEq | Assign

data CallExpr = CallExpr Identifier [Expr]

data Lit = D Double | I Int

-- simplification call xpr is valid ?
data Postfix = C CallExpr | L Lit | Id Identifier | E Exprs -- carefull Grammar Exprs is between parenthesis

-- infinity of unop before postix ?
data Unary = UNOP Unop Unary | P Postfix

-- data Expr = U Unary | OP1 Unary [(Binop, Unary)] | OP2 Unary [(Binop, Expr)]
data Expr = OP1 Unary [(Binop, Expr)]

data Exprs = Exprs [Expr] -- ++ For, if, else, while

-- data While = (, , , Expr)
data For = For (Identifier, Expr) (Identifier, Expr) Expr Exprs -- init i, cmp, it++, block

data While = While Expr Exprs

data If = IfThen Expr Exprs | IfElse Expr Exprs Exprs

data TypeKoak = INT | DOUBLE | VOID

data Args = Arg [(TypeKoak, Lit)]

data Proto = Proto Identifier Args TypeKoak -- ++ Unary, Binary

data Def = Def Proto Expr

data KDef = Df Def | Expr Exprs

data AST = AST [KDef]
