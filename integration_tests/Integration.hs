module Integration where

import LLVM_Module
import DataType2

mod1:: Expr -- ok
mod1 = (Protof (Typed "fct1" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Typed "y" INT) (VAL (I 5)) ) ), (Id (Typed "y" INT) )] ))

-- mod2:: Expr -- ok
-- mod2 = (Protof (Typed "fct2" INT) 
--             [(Typed "a" INT), (Typed "b" INT)] 
--             (Exprs [(Val (I 6))] ))

mod3:: Expr -- ok
mod3 = (Protof (Typed "fct1" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Id (Typed "a" INT) )]))


mod4 :: Expr -- ok
mod4 = (Protof (Typed "fct1" INT) 
            [(Typed "a" INT), (Typed "b" INT)] 
            (Exprs [(Operation (ASSIGN (Typed "y" INT) (ADD [(XPR (Id (Typed "a" INT))), (VAL (I 5))]) ) ), (Id (Typed "y" INT) )] ))

main = do 
    -- genObjFromExpr "mod1" [mod3]
    genObjFromExpr "mod1" [mod4]
    -- genObjFromExpr "mod2" [mod2]
    -- genObjFromExpr "mod3" [mod3]
