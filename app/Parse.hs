--
-- EPITECH PROJECT, 2020
-- B-YEP-500-PAR-5-1-koak-aurele.auboin
-- File description:
-- Parse.hs
--

module Parse where

import MyParser
import DataType2
import Control.Applicative
import Parse2

char :: Char -> Parser Char
char c = Parser (\str -> runParser (parseWSpace (parseChar c)) str)

chars :: String -> Parser Char
chars s = Parser (\str -> runParser (parseWSpace (parseAnyChar s)) str)

word :: String -> Parser String
word w = Parser (\str -> runParser (parseWSpace (parseWord w)) str)

recu :: Parser Expr
recu = Parser (\str -> runParser (parseWSpace parseMain) str)

makeArray :: Expr -> Expr
makeArray (Exprs r) = (Exprs r)
makeArray r = (Exprs [r])

arrRecu :: Parser Expr
arrRecu = Parser (\str -> runParser (makeArray <$> recu) str)

parseId :: Parser Identifier
parseId = Parser (\str -> runParser (parseWSpace (Wait <$> parseId2)) str)

parseIf :: Parser Expr
parseIf = Parser (\str -> runParser mainIf str)
    where
        mainIf = parIf <|> ifExpr
        parIf = ((char '(')*> ifExpr <* (char ')'))
        ifExpr = parseWSpace (ifElse <|> ifThen)
        ifElse = (IfElse <$> cond <*> iThen <*> ((word "else") *> arrRecu))
        ifThen = (IfThen <$> cond <*> iThen)
        cond = ((word "if") *> recu)
        iThen = ((word "then") *> arrRecu)

parseWhile :: Parser Expr
parseWhile = Parser (\str -> runParser mainWhile str)
    where
        mainWhile = parWhile <|> while
        parWhile = ((char '(')*> while <* (char ')'))
        while = (While <$> ((word "while") *> recu) <*> content)
        content = ((word "do") *> arrRecu)

toTuple :: Identifier -> Expr -> (Identifier, Expr)
toTuple id expr = (id, expr)

for2 :: Parser (Identifier, Expr)
for2 = Parser (\str -> runParser convert str)
    where
        convert = parseWSpace (toTuple <$> (parseId <* (char '<')) <*> inf)
        inf = (recu <* (char ','))

for1 :: Parser (Identifier, Expr)
for1 = Parser (\str -> runParser convert str)
    where
        convert = parseWSpace (toTuple <$> iden <*> (recu <* (char ',')))
        iden = ((word "for") *> parseId <* (char '='))

parseFor :: Parser Expr
parseFor = Parser (\str -> runParser mainFor str)
    where
        mainFor = parFor <|> for
        parFor = ((char '(')*> for <* (char ')'))
        for = (For <$> for1 <*> for2 <*> for3 <*> arrRecu)
        for3 = parseWSpace (parseAndWith (\a b -> a) recu (word "in"))

parseUnop :: Parser Unop
parseUnop = Parser (\str -> case runParser (char '!') str of
    Nothing -> case runParser (char '-') str of
        Nothing -> Nothing
        Just (_, r) -> Just (UMinus, r)
    Just (_, r) -> Just (Not, r)
    )

simplifyOp :: Expr -> Expr
simplifyOp (Operation (XPR (Val v))) = (Val v)
simplifyOp (Operation (VAL v)) = (Val v)
simplifyOp (Operation (XPR (Id id))) = (Id id)
simplifyOp (Operation (XPR (Unary u r))) = (Unary u r)
simplifyOp (Operation (XPR (Callf n a))) = (Callf n a)
simplifyOp r = r

parseType :: Parser TypeKoak
parseType = Parser (\str -> case runParser (word "int") str of
    Nothing -> case runParser (word "double") str of
        Nothing -> Nothing
        Just (_, r) -> Just (DOUBLE, r)
    Just (_, r) -> Just (INT, r)
    )

typeVar :: Parser TypeKoak
typeVar = Parser (\str -> runParser argType str)
    where
        argType = parseWSpace (parseType)

parseArg :: Parser Identifier
parseArg = Parser (\str -> runParser argument str)
    where
        argument = parseWSpace (Typed <$> (parseId2 <* (char ':')) <*> typeVar)

argArrEndPar :: [Identifier] -> Parser [Identifier]
argArrEndPar array = Parser (\str -> case runParser (char ')') str of
    Nothing -> Nothing
    (Just (')', r)) -> Just (array, r))

argArrInitArr :: Identifier -> Parser [Identifier]
argArrInitArr id = Parser (\r -> case runParser (argArr (Just [id])) r of
    Nothing -> Just ([id], r)
    res -> res)

argArr :: Maybe [Identifier] -> Parser [Identifier]
argArr Nothing = Parser (\str -> Nothing)
argArr (Just []) = Parser (\str -> case runParser (parseArg) str of
    Nothing -> Nothing
    (Just (id, r)) -> runParser (argArrInitArr id) r)
argArr (Just array) = Parser (\str->case runParser ((char ',')*>parseArg) str of
    Nothing -> runParser (argArrEndPar array) str
    (Just (id, r)) -> case runParser (argArr (Just (array ++ [id]))) r of
        Nothing -> Just ((array ++ [id]), r)
        res -> res)

extractFunc :: String -> [Identifier] -> TypeKoak -> Expr -> Expr
extractFunc id args typ def = Protof (Typed id typ) args def

emptyIdArr :: Parser [Identifier]
emptyIdArr = Parser(\str -> Just([], str))

definition :: Parser Expr
definition = Parser (\str -> case runParser (word "def") str of
    Nothing -> Nothing
    _ -> case runParser def str of
        Nothing -> Just (Err "invalid definition", str)
        r -> r)
    where
        def = extractFunc<$>a<*>(b<*char ':')<*>typeVar<*>(arrRecu<*char ';')
        a = ((word "def") *> (parseWSpace parseId2) <* (char '('))
        b = (((char ')') *> emptyIdArr) <|> (parseWSpace (argArr (Just []))))

assignOp3 :: Op -> Char -> Op -> Op
assignOp3 single '-' opR = case opR of
    SUB arr -> SUB (single:arr)
    ADD (a:b) -> ADD ((assignOp single '-' a):b)
    any -> SUB (single:any:[])
assignOp3 single '+' opR = case opR of
    ADD arr -> ADD (single:arr)
    any -> ADD (single:any:[])

assignOp2 :: Op -> Char -> Op -> Op
assignOp2 single '/' opR = case opR of
    DIV arr -> DIV (single:arr)
    SUB (a:b) -> SUB ((assignOp single '/' a):b)
    ADD (a:b) -> ADD ((assignOp single '/' a):b)
    any -> DIV (single:any:[])
assignOp2 a b c = assignOp3 a b c

assignOp :: Op -> Char -> Op -> Op
assignOp single '*' opR = case opR of
    MUL arr -> MUL (single:arr)
    DIV (a:b) -> DIV ((assignOp single '*' a):b)
    SUB (a:b) -> SUB ((assignOp single '*' a):b)
    ADD (a:b) -> ADD ((assignOp single '*' a):b)
    any -> MUL (single:any:[])
assignOp a b c = assignOp2 a b c

parseOpSign :: Parser Op
parseOpSign = Parser (\str -> runParser opPiece str)
    where
        opPiece = withSign <|> single
        withSign = (assignOp <$> single <*> (chars "*/-+") <*> parseOpSign)
        single = parseWSpace((PAR<$>(char '('*>parseOneOp<*char ')')) <|> simp)
        simp = parseWSpace (unary <|> call <|> ((XPR<$>Id<$>parseId)) <|> valu)
        unary = (XPR<$>(Unary<$>parseUnop<*>(simplifyOp<$>Operation<$>single)))
        call = (XPR <$> (parseWSpace parseCall))
        valu = ((VAL <$> D <$> parseDouble2) <|> (VAL <$> I <$> parseInte))

assignComp :: Op -> String -> Op -> Op
assignComp op1 "<" op2 = DataType2.LT op1 op2
assignComp op1 ">" op2 = DataType2.GT op1 op2
assignComp op1 "==" op2 = DataType2.EQ op1 op2
assignComp op1 "!=" op2 = NOTEQ op1 op2

parseComp :: Parser Op
parseComp = Parser (\str -> runParser opComp str)
    where
        opComp = (assignComp <$> parseOpSign <*> comps <*> parseOpSign)
        comps = parseWSpace (parseAnyStr ("<":">":"==":"!=":[]))

parseOneOp :: Parser Op
parseOneOp = Parser (\str -> runParser allOp str)
    where
        allOp = assign <|> comp <|> sign <|> call <|> id <|> valu
        assign = parseWSpace (ASSIGN <$> (parseId <* (char '=')) <*> parseOneOp)
        sign = parseWSpace (parseOpSign)
        call = (XPR <$> parseCall)
        valu = parseWSpace ((VAL<$>D<$>parseDouble2) <|> (VAL<$>I<$> parseInte))
        id = (parseWSpace (XPR <$> Id <$> parseId))
        comp = parseWSpace (parseComp)

removeParArr :: [Op] -> [Op]
removeParArr [] =  []
removeParArr (x:xs) = (removePar x):removeParArr xs

removePar2 :: Op -> Op
removePar2 (DataType2.LT a b) = DataType2.LT (removePar a) (removePar b)
removePar2 (DataType2.GT a b) = DataType2.GT (removePar a) (removePar b)
removePar2 (DataType2.EQ a b) = DataType2.EQ (removePar a) (removePar b)
removePar2 (NOTEQ a b) = NOTEQ (removePar a) (removePar b)
removePar2 (ASSIGN var op) = (ASSIGN var (removePar op))
removePar2 op = op

removePar :: Op -> Op
removePar (PAR op) = removePar op
removePar (ADD arr@(x:xs)) = ADD (removeParArr arr)
removePar (SUB arr@(x:xs)) = SUB (removeParArr arr)
removePar (MUL arr@(x:xs)) = MUL (removeParArr arr)
removePar (DIV arr@(x:xs)) = DIV (removeParArr arr)
removePar op = removePar2 op

parseOp :: Parser Expr
parseOp = Parser (\str->runParser (Operation <$> removePar <$> parseOneOp) str)

wrapperParseOp :: Parser Expr
wrapperParseOp = Parser (\str -> runParser (simplifyOp <$> parseOp) str)

checkCallArgPar :: [Expr] -> Parser [Expr]
checkCallArgPar arr = Parser (\str -> case runParser (char ')') str of
    Nothing -> Nothing
    Just (')', r) -> Just (arr, r)
    )

callArg :: Maybe [Expr] -> Parser [Expr]
callArg Nothing = Parser (\str -> Nothing)
callArg (Just []) = Parser (\str -> case runParser (recu) str of
    Nothing -> runParser (checkCallArgPar []) str
    Just (xpr, r) -> runParser (callArg (Just (xpr:[]))) r)
callArg (Just array) = Parser (\str->case runParser ((char ',') *> recu) str of
    Nothing -> runParser (checkCallArgPar array) str
    (Just (xpr, r)) -> runParser (callArg (Just (array ++ [xpr]))) r)

parseCall :: Parser Expr
parseCall = Parser (\str -> runParser call str)
    where
        call = parseWSpace (Callf <$> (parseId <* (char '(')) <*> call2)
        call2 = parseWSpace (callArg (Just []))

initArray :: Expr -> [Expr]
initArray a = a:[]

addArray :: Expr -> [Expr] -> [Expr]
addArray a arr = a:arr

parseMain :: Parser Expr
parseMain = Parser (\str -> runParser (parseAll) str)
    where
        parseAll = exprs <|> oneExpr
        oneExpr = builtIn <|> parseCall <|> wrapperParseOp
        builtIn = (parseFor <|> parseWhile <|> parseIf)
        exprs = Exprs<$>(addArray<$> (oneExpr <* (char ':')) <*> (next <|> end))
        next = (addArray <$> (oneExpr <* (char ':')) <*> (next <|> end))
        end = (initArray <$> oneExpr)

globalVar :: Parser Expr
globalVar = Parser(\str -> case runParser (parseMain <* (char ';')) str of
    regAssign@(Just (Operation (ASSIGN (Wait _) (VAL _)), _)) -> regAssign
    unaryAssign@(Just (Operation (ASSIGN (Wait _) (XPR (Unary _ (Val _)))), _)) -> unaryAssign
    _ -> Nothing
    )

parse :: Parser Expr
parse = Parser (\str -> case runParser (parseExtSpaces parseAll) str of
    Just (r, []) -> Just (r, [])
    _ -> Nothing)
    where
        parseAll = definition <|> globalVar