module StatementHelper where

-- import LLVM_Module
-- import LLVM_Builder
import DataType
-- import BuilderState
-- import StatementHelper
-- import LLVM_Block
-- import Data.Maybe
-- import Control.Monad.State
-- import LLVM.AST.Name

ifElseElem :: Expr
ifElseElem = (IfElse (Operation (DataType.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Exprs [(Id (Typed "a" INT))]) (Exprs [(Val (I (-11)))])  )

ifElseElem2 :: Expr
ifElseElem2 = (IfElse (Operation
                (DataType.GT (XPR (Id (Typed "a" INT))) (VAL (I 5)) ))
                    (Exprs [assignElem, assignElem])
                        (Exprs [assignElem, assignElem]))

ifElseElem3 :: Expr
ifElseElem3 = (IfElse (Operation
                (DataType.GT (XPR (Id (Typed "a" INT))) (VAL (I 5)) ))
                    (Exprs [simpleAdd, simpleAdd])
                        (Exprs [simpleAdd]))

-- block imbrication
ifElseElem4 :: Expr
ifElseElem4 = (IfElse (Operation
                (DataType.EQ (XPR (Id (Typed "a" INT))) (VAL (I 1)) ))
                    (Exprs [ifElseElem2, assignElem])
                        (Exprs [assignElem, assignElem]))

ifElseElem5 :: Expr
ifElseElem5 = (IfElse (Operation
                (DataType.EQ (XPR (Id (Typed "a" INT))) (VAL (I 1)) ))
                    (Exprs [ifElseElem3, simpleAdd])
                        (Exprs [simpleAdd, simpleAdd])
                        )

simpleAdd :: Expr
simpleAdd = (Operation (ADD [(VAL (I 7)), (VAL (I 90))]))

assignElem ::Expr
assignElem = (Operation (ASSIGN (Typed "b" INT) (VAL (I 5))))

assignB :: Integer -> Expr
assignB val = (Operation (ASSIGN (Typed "b" INT) (VAL (I val))))

addA1 :: Expr
addA1 = (Operation (ASSIGN (Typed "a" INT) (ADD [(XPR (Id (Typed "a" INT))), (VAL (I 1))] )) )

addINT :: String -> Expr
addINT str = (Operation (ASSIGN (Typed str INT) (ADD [(XPR (Id (Typed str INT))), (VAL (I 1))] )) )

addB1 :: Expr
addB1 = (Operation (ASSIGN (Typed "b" INT) (ADD [(XPR (Id (Typed "b" INT))), (VAL (I 1))] )) )

condition :: Expr
condition = (Operation (DataType.LT (XPR (Id (Typed "a" INT))) (VAL (I 90))) )

whileElem :: [Expr] -> Expr
whileElem xprs = (While condition (Exprs xprs))

paramFact :: Expr
paramFact = (Operation (SUB [(XPR (Id (Typed "a" INT))), (VAL (I 1))]))

ifElseFact :: Expr
ifElseFact = (IfElse (Operation (DataType.LT (XPR (Id (Typed "a" INT))) (VAL (I 2)))) (Exprs [(Val (I 1))]) (Exprs [(Operation (MUL [(XPR (Id (Typed "a" INT))), (XPR (Callf (Typed "fact" INT) [paramFact]))] ))]) )

-- while () if else ints

ifGenericIf :: Expr
ifGenericIf = (IfElse (Operation (DataType.GT (XPR (Id (Typed "a" INT))) (VAL (I 2)))) (Exprs [(Val (I 1))]) (Exprs [(Val (I 2))]))

whileCtr :: [Expr] -> Expr
whileCtr xprs = (While (Operation (DataType.LT (XPR (Id (Typed "a" INT))) (VAL (I 5)) )) (Exprs xprs))

fctWrapper :: String -> [Expr] -> Expr
fctWrapper str xprs = (Protof (Typed str INT) [(Typed "a" INT)] (Exprs xprs))


-- generic function

assignA :: Integer -> Expr
assignA val = (Operation (ASSIGN (Typed "a" INT) (VAL (I val))))

-- used
cond1 :: Expr
cond1 = (Operation (DataType.EQ (XPR (Id (Typed "a" INT))) (VAL (I 5)) ))

cond2 :: Expr
cond2 = (Operation (DataType.NOTEQ (XPR (Id (Typed "a" INT))) (VAL (I 5)) ))

condLT :: String -> Integer -> Expr
condLT varName limit = (Operation (DataType.LT (XPR (Id (Typed varName INT))) (VAL (I limit)) ))

genericIfElse :: [Expr] -> [Expr] -> Expr
genericIfElse xprs xprs2 = (IfElse cond1 (Exprs xprs) (Exprs xprs2))

genericIfThen :: [Expr] -> Expr
genericIfThen xprs = (IfThen cond1 (Exprs xprs))

genericWhile :: Expr -> [Expr] -> Expr
genericWhile cond xprs = (While cond (Exprs xprs))

cond3 :: Expr
cond3 = (Operation (DataType.LT (XPR (Id (Typed "b" INT))) (VAL (I 5)) ))

genericFor :: [Expr] -> Expr
genericFor xprs = (For ((Typed "b" INT), (Val (I 0))) ((Typed "b" INT), (Val (I 5))) (addB1) (Exprs xprs))
