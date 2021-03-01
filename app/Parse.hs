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

word :: String -> Parser String
word w = Parser (\str -> runParser (parseWSpace (parseWord w)) str)

recu :: Parser Expr
recu = Parser (\str -> runParser (parseWSpace parseMain) str)

makeArray :: Expr -> Expr
makeArray (Exprs r) = (Exprs r)
makeArray r = (Exprs [r])

arrRecu :: Parser Expr
arrRecu = Parser (\str -> runParser (makeArray <$> recu) str)

-- recu :: Parser Expr
-- recu = Parser (\str -> Just ((Val (I 9)), str))


parseId :: Parser Identifier
parseId = Parser (\str -> runParser (parseWSpace (Wait <$> parseId2)) str)

parseIf :: Parser Expr
parseIf = Parser (\str -> runParser ifExpr str)
    where
        ifExpr = ifElse <|> ifThen
        ifElse = parseWSpace (IfElse <$> ignoreIf <*> ((word "then") *> arrRecu) <*> ((word "else") *> arrRecu))
        ifThen = parseWSpace (IfThen <$> ignoreIf <*> ((word "then") *> arrRecu))
        ignoreIf = parseAndWith (\_ b -> b) (word "if") recu

parseWhile :: Parser Expr
parseWhile = Parser (\str -> runParser while str)
    where
        while = parseWSpace (While <$> ((word "while") *> recu) <*> ((word "do") *> arrRecu)) -- while i < 9 do expr;

toTuple :: Identifier -> Expr -> (Identifier, Expr)
toTuple id expr = (id, expr)

-- for2 :: Parser (Identifier, Expr)
-- for2 = Parser (\str -> runParser convert str)
--     where
--         convert = parseWSpace (toTuple <$> parseId <* (char '<') *> recu <* (char ','))

for2 :: Parser (Identifier, Expr)
for2 = Parser (\str -> runParser convert str)
    where
        convert = parseWSpace (toTuple <$> (parseId <* (char '<')) <*> (recu <* (char ',')))

for1 :: Parser (Identifier, Expr)
for1 = Parser (\str -> runParser convert str)
    where
        convert = parseWSpace (toTuple <$> (ignoreFor <* (char '=')) <*> (recu <* (char ',')))
        ignoreFor = parseAndWith (\_ b -> b) (word "for") parseId

parseFor :: Parser Expr
parseFor = Parser (\str -> runParser for str)
    where
        for = parseWSpace (For <$> for1 <*> for2 <*> for3 <*> arrRecu) -- for x = 1, x < 9, (1 or x = x + 1) in expr;
        for3 = parseWSpace (parseAndWith (\a b -> a) recu (word "in"))

parseUnop :: Parser Unop
parseUnop = Parser (\str -> case runParser (char '!') str of
    Nothing -> case runParser (char '-') str of
        Nothing -> Nothing
        Just (_, r) -> Just (UMinus, r)
    Just (_, r) -> Just (Not, r)
    )

-- parseUnary :: Parser Expr
-- parseUnary = Parser(\str -> runParser unary str)
--     where
--         unary = parseSpaces (Unary <$> unop <*> recu)
--         unop = parseSpaces (parseUnop)

simplifyOp :: Expr -> Expr
simplifyOp (Operation (XPR (Val v))) = (Val v)
simplifyOp (Operation (VAL v)) = (Val v)
simplifyOp (Operation (XPR (Id id))) = (Id id)
simplifyOp (Operation (XPR (Unary u r))) = (Unary u r)
simplifyOp r = r

-- parseOpUnary :: Parser Expr
-- parseOpUnary = Parser(\str -> runParser unary str)
--     where
--         unary = parseWSpace (Unary <$> (parseWSpace (parseUnop)) <*> (simplifyOp <$> Operation <$> parseOpSign))
--         unop = 


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
        argument = parseWSpace (Typed <$> (parseLetters <* (char ':')) <*> typeVar)

argArr :: Maybe [Identifier] -> Parser [Identifier]
argArr Nothing = Parser (\str -> Nothing)
argArr (Just []) = Parser (\str -> case runParser (parseArg) str of
    Nothing -> Nothing
    (Just (id, r)) -> case runParser (argArr (Just [id])) r of
        Nothing -> Just ([id], r)
        res -> res
    )
argArr (Just array) = Parser (\str -> case runParser (parseAndWith (\_ l -> l) (char ',') parseArg) str of
    Nothing -> case runParser (char ')') str of
        Nothing -> Nothing
        (Just (')', r)) -> Just (array, r)
    (Just (id, r)) -> case runParser (argArr (Just (array ++ [id]))) r of
        Nothing -> Just ((array ++ [id]), r)
        res -> res
    )

extractFunc :: String -> [Identifier] -> TypeKoak -> Expr -> Expr
extractFunc id args typ def = Protof (Typed id typ) args def

definition :: Parser Expr
definition = Parser (\str -> case runParser (word "def") str of
    Nothing -> Nothing
    _ -> case runParser def str of
        Nothing -> Just (Err "invalid definition", str)
        r -> r
    )
    where
        def = parseWSpace (extractFunc <$> proto1 <*> (proto2 <* (char ':')) <*> typeVar <*> arrRecu <* (char ';'))
        b = parseAndWith (\_ b -> b)
        proto1 = ((word "def") *> (parseWSpace parseLetters) <* (char '('))
        proto2 = parseWSpace (argArr (Just []))

assignOp :: Op -> Char -> Op -> Op
assignOp single '*' opR = case opR of
    MUL arr -> MUL (single:arr)
    DIV (a:b) -> DIV ((assignOp single '*' a):b)
    SUB (a:b) -> SUB ((assignOp single '*' a):b)
    ADD (a:b) -> ADD ((assignOp single '*' a):b)
    any -> MUL (single:any:[])
assignOp single '/' opR = case opR of
    DIV arr -> DIV (single:arr)
    SUB (a:b) -> SUB ((assignOp single '/' a):b)
    ADD (a:b) -> ADD ((assignOp single '/' a):b)
    any -> DIV (single:any:[])
assignOp single '-' opR = case opR of
    SUB arr -> SUB (single:arr)
    ADD (a:b) -> ADD ((assignOp single '-' a):b)
    any -> SUB (single:any:[])
assignOp single '+' opR = case opR of
    ADD arr -> ADD (single:arr)
    any -> ADD (single:any:[])

parseOpSign :: Parser Op
parseOpSign = Parser (\str -> runParser opPiece str)
    where
        opPiece = withSign <|> single
        withSign = parseWSpace (assignOp <$> single <*> (parseWSpace (parseAnyChar "*/-+")) <*> parseOpSign)
        single = parseWSpace (par <|> simp)
        par = (PAR <$> ((char '(') *> (parseOneOp) <* (char ')')))
        simp = (unary <|> call <|> id <|> valu)
        unary = parseWSpace (XPR <$> (Unary <$> (parseWSpace (parseUnop)) <*> (simplifyOp <$> Operation <$> single)))
        call = (XPR <$> (parseWSpace parseCall))
        valu = parseWSpace ((VAL <$> D <$> parseDouble2) <|> (VAL <$> I <$> parseInte))
        id = (XPR <$> Id <$> parseId)

assignComp :: Op -> String -> Op -> Op
assignComp op1 "<" op2 = DataType2.LT op1 op2
assignComp op1 ">" op2 = DataType2.GT op1 op2
assignComp op1 "==" op2 = DataType2.EQ op1 op2
assignComp op1 "!=" op2 = NOTEQ op1 op2

parseComp :: Parser Op
parseComp = Parser (\str -> runParser opComp str)
    where
        opComp = parseWSpace (assignComp <$> parseOpSign <*> comps <*> parseOpSign)
        comps = parseWSpace (parseAnyStr ("<":">":"==":"!=":[]))

parseOneOp :: Parser Op
parseOneOp = Parser (\str -> runParser allOp str)
    where
        allOp = assign <|> comp <|> sign <|> call <|> id <|> valu
        assign = parseWSpace (ASSIGN <$> (parseId <* (char '=')) <*> parseOneOp)
        sign = parseWSpace (parseOpSign)
        call = (XPR <$> parseCall)
        valu = parseWSpace ((VAL <$> D <$> parseDouble2) <|> (VAL <$> I <$> parseInte))
        id = (parseWSpace (XPR <$> Id <$> parseId))
        comp = parseWSpace (parseComp)

removeParArr :: [Op] -> [Op]
removeParArr [] =  []
removeParArr (x:xs) = (removePar x):removeParArr xs

removePar :: Op -> Op
removePar (PAR op) = removePar op
removePar (ADD arr@(x:xs)) = ADD (removeParArr arr)
removePar (SUB arr@(x:xs)) = SUB (removeParArr arr)
removePar (MUL arr@(x:xs)) = MUL (removeParArr arr)
removePar (DIV arr@(x:xs)) = DIV (removeParArr arr)
removePar (DataType2.LT a b) = DataType2.LT (removePar a) (removePar b)
removePar (DataType2.GT a b) = DataType2.GT (removePar a) (removePar b)
removePar (DataType2.EQ a b) = DataType2.EQ (removePar a) (removePar b)
removePar (NOTEQ a b) = NOTEQ (removePar a) (removePar b)
removePar (ASSIGN var op) = (ASSIGN var (removePar op))
removePar op = op

parseOp :: Parser Expr
parseOp = Parser (\str -> runParser (Operation <$> removePar <$> parseOneOp) str)

wrapperParseOp :: Parser Expr
wrapperParseOp = Parser (\str -> runParser (simplifyOp <$> parseOp) str)

callArg :: Maybe [Expr] -> Parser [Expr]
callArg Nothing = Parser (\str -> Nothing)
callArg (Just []) = Parser (\str -> case runParser (recu) str of
    Nothing -> Nothing
    Just (xpr, r) -> runParser (callArg (Just (xpr:[]))) r
    )
callArg (Just array) = Parser (\str -> case runParser (parseAndWith (\_ l -> l) (char ',') recu) str of
    Nothing -> case runParser (char ')') str of
        Nothing -> Nothing
        (Just (')', r)) -> Just (array, r)
    (Just (xpr, r)) -> runParser (callArg (Just (array ++ [xpr]))) r
    )

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
        exprs = Exprs <$> (addArray <$> (oneExpr <* (char ':')) <*> (nextExprs <|> endExprs))
        nextExprs = (addArray <$> (oneExpr <* (char ':')) <*> (nextExprs <|> endExprs))
        endExprs = (initArray <$> oneExpr)

wrapperGlobalVariable :: Parser Expr
wrapperGlobalVariable = Parser (lbd psr)
        where
        psr = parseMain <* (char ';')
        lbd = (\psr str -> 
                case (runParser psr str) of
                ass@(Just (Operation (ASSIGN (Wait _) (VAL _)), _)) -> ass
                _ -> Nothing)

parse :: Parser Expr
parse = Parser (\str -> runParser (parseAll) str)
    where
        parseAll = definition <|> wrapperGlobalVariable
        -- id = parseWSpace (Id <$> (parseId))


-- parseLine :: Parser Expr
-- parseLine = Parser (\str -> case runParser (parse getLine))

-- recSplit :: Parser [Expr]
-- recSplit = Parser (\str -> case str of
--     [] -> Just([], [])
--     s -> runParser ((\ x y -> x:y) <$> splitLine <*> recSplit) s
--     )

-- parseFile :: String -> Maybe [Expr]
-- parseFile [] = Just []
-- parseFile content = case runParser recSplit content of
--     Just (expr, r) -> Just expr
--     Nothing -> Nothing

-- mergeMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
-- mergeMaybe Nothing b = Nothing
-- mergeMaybe a Nothing = Nothing
-- mergeMaybe (Just a) (Just b) = Just (a ++ b)

-- parseFiles :: [String] -> Maybe [Expr]
-- parseFiles [] = Just []
-- parseFiles (file:xs) = mergeMaybe (parseFile file) (parseFiles xs)

-- getContent :: [String] -> IO(Maybe [String])
-- getContent [] = return (Just [])
-- getContent (file:r) = do
--     b <- doesFileExist file
--     case b of
--         False -> return Nothing
--         True -> do
--             f <- readFile file
--             rest <- getContent r
--             case rest of
--                 Just list -> return $ Just (f:list)
--                 Nothing -> return Nothing

-- main :: IO()
-- main = do
--     args <- getArgs
--     case args of
--         [] -> hPutStrLn stderr "The REPL is not implemented" >>
--             exitWith (ExitFailure 84)
--         _ -> do
--             argRes <- parseArgs args
--             case argRes of
--                 84 -> exitWith (ExitFailure 84)
--                 _ -> do
--                     contents <- getContent args
--                     case contents of
--                         Nothing -> hPutStrLn stderr "Non-existant file" >>
--                             exitWith (ExitFailure 84)
--                         Just cont -> case parseFiles cont of
--                             Nothing -> hPutStrLn stderr "The files are invalid" >>
--                                 exitWith (ExitFailure 84)
--                             Just expr -> putStrLn (evalExpr expr)
