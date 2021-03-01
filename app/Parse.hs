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
char c = Parser (\str -> runParser (parseSpaces (parseChar c)) str)

word :: String -> Parser String
word w = Parser (\str -> runParser (parseSpaces (parseWord w)) str)

recu :: Parser Expr
recu = Parser (\str -> runParser (parseSpaces parse) str)

-- recu :: Parser Expr
-- recu = Parser (\str -> Just ((Val (I 9)), str))


parseId :: Parser Identifier
parseId = Parser (\str -> runParser (parseSpaces (Wait <$> parseId2)) str)

parseIf :: Parser Expr
parseIf = Parser (\str -> runParser ifExpr str)
    where
        ifExpr = ifElse <|> ifThen
        ifElse = parseSpaces (IfElse <$> ignoreIf <*> ((word "then") *> recu) <*> ((word "else") *> recu))
        ifThen = parseSpaces (IfThen <$> ignoreIf <*> ((word "then") *> recu))
        ignoreIf = parseAndWith (\_ b -> b) (word "if") recu

parseWhile :: Parser Expr
parseWhile = Parser (\str -> runParser while str)
    where
        while = parseSpaces (While <$> ((word "while") *> recu) <*> ((word "do") *> recu)) -- while i < 9 do expr;

toTuple :: Identifier -> Expr -> (Identifier, Expr)
toTuple id expr = (id, expr)

-- for2 :: Parser (Identifier, Expr)
-- for2 = Parser (\str -> runParser convert str)
--     where
--         convert = parseSpaces (toTuple <$> parseId <* (char '<') *> recu <* (char ','))

for2 :: Parser (Identifier, Expr)
for2 = Parser (\str -> runParser convert str)
    where
        convert = parseSpaces (toTuple <$> (parseId <* (char '<')) <*> (recu <* (char ',')))

for1 :: Parser (Identifier, Expr)
for1 = Parser (\str -> runParser convert str)
    where
        convert = parseSpaces (toTuple <$> (ignoreFor <* (char '=')) <*> (recu <* (char ',')))
        ignoreFor = parseAndWith (\_ b -> b) (word "for") parseId

parseFor :: Parser Expr
parseFor = Parser (\str -> runParser for str)
    where
        for = parseSpaces (For <$> for1 <*> for2 <*> for3 <*> recu) -- for x = 1, x < 9, (1 or x = x + 1) in expr;
        for3 = parseSpaces (parseAndWith (\a b -> a) recu (word "in"))

parseUnop :: Parser Unop
parseUnop = Parser (\str -> case runParser (char '!') str of
    Nothing -> case runParser (char '-') str of
        Nothing -> Nothing
        Just (_, r) -> Just (UMinus, r)
    Just (_, r) -> Just (Not, r)
    )

parseUnary :: Parser Expr
parseUnary = Parser(\str -> runParser unary str)
    where
        unary = parseSpaces (Unary <$> unop <*> recu)
        unop = parseSpaces (parseUnop)

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
        argType = parseSpaces (parseType)

parseArg :: Parser Identifier
parseArg = Parser (\str -> runParser argument str)
    where
        argument = parseSpaces (Typed <$> (parseLetters <* (char ':')) <*> typeVar)

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
        def = parseSpaces (extractFunc <$> proto1 <*> (proto2 <* (char ':')) <*> typeVar <*> recu <* (char ';'))
        b = parseAndWith (\_ b -> b)
        proto1 = ((word "def") *> (parseSpaces parseLetters) <* (char '('))
        proto2 = parseSpaces (argArr (Just []))

-- before : 3 * 4 / 5 / 6 -> [3*[4/5/6]]
-- after  : 3 * 4 / 5 / 6 -> [[3*4]/5/6]
assignOp :: Op -> Char -> Op -> Op
assignOp single '*' opR = case opR of
    MUL arr -> MUL (single:arr)
    DIV (a:b) -> DIV ((MUL (single:a:[])):b)
    SUB (a:b) -> SUB ((MUL (single:a:[])):b)
    ADD (a:b) -> ADD ((MUL (single:a:[])):b)
    any -> MUL (single:any:[])
assignOp single '/' opR = case opR of
    DIV arr -> DIV (single:arr)
    SUB (a:b) -> SUB ((DIV (single:a:[])):b)
    ADD (a:b) -> ADD ((DIV (single:a:[])):b)
    any -> DIV (single:any:[])
assignOp single '-' opR = case opR of
    SUB arr -> SUB (single:arr)
    ADD (a:b) -> ADD ((SUB (single:a:[])):b)
    any -> SUB (single:any:[])
assignOp single '+' opR = case opR of
    ADD arr -> ADD (single:arr)
    any -> ADD (single:any:[])

parseOpSign :: Parser Op
parseOpSign = Parser (\str -> runParser opPiece str)
    where
        opPiece = withSign <|> without
        withSign = parseSpaces (assignOp <$> single <*> (parseSpaces (parseAnyChar "*/-+")) <*> parseOpSign)
        without = single
        single = (par <|> simp)
        -- v _ r _ = r
        par = ((char '(') *> (parseOneOp) <* (char ')'))
        simp = (valu <|> call)
        call = (XPR <$> (parseSpaces parseMainOp))
        -- call = Parser (\str -> Just ((XPR (Val (I 3))), str))
        valu = parseSpaces ((VAL <$> D <$> parseDouble2) <|> (VAL <$> I <$> parseInte))

assignComp :: Op -> String -> Op -> Op
assignComp op1 "<" op2 = DataType2.LT op1 op2
assignComp op1 ">" op2 = DataType2.GT op1 op2
assignComp op1 "==" op2 = DataType2.EQ op1 op2
assignComp op1 "!=" op2 = NOTEQ op1 op2

parseComp :: Parser Op
parseComp = Parser (\str -> runParser opComp str)
    where
        opComp = parseSpaces (assignComp <$> parseOpSign <*> comps <*> parseOpSign)
        comps = parseSpaces (parseAnyStr ("<":">":"==":"!=":[]))

parseOneOp :: Parser Op
parseOneOp = Parser (\str -> runParser allOp str)
    where
        allOp = assign <|> comp <|> sign <|> call <|> valu
        assign = parseSpaces (ASSIGN <$> (parseId <* (char '=')) <*> parseOneOp)
        sign = parseSpaces (parseOpSign)
        call = (XPR <$> (parseSpaces parseMainOp))
        -- call = Parser (\str -> Just ((XPR (Val (I 9))), str))
        valu = parseSpaces ((VAL <$> D <$> parseDouble2) <|> (VAL <$> I <$> parseInte))
        comp = parseSpaces (parseComp)

-- parseOp :: Parser Expr
-- parseOp = Parser (\str -> runParser (Operation <$> parseOneOp) str)

parseOp :: Parser Expr
parseOp = Parser (\str -> runParser (Operation <$> parseOneOp) str)

wrapperParseOp :: Parser Expr
wrapperParseOp = Parser (lbd parseOp)
            where
            lbd = (\psr str -> 
                    case runParser psr str of
                    Just ((Operation (XPR (Val v))), rest) -> Just ((Val v), rest)
                    Just ((Operation (VAL v)), rest) -> Just ((Val v), rest)
                    Just ((Operation (XPR (Id id))), rest) -> Just ((Id id), rest)
                    xpr -> xpr)

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
        call = parseSpaces (Callf <$> (parseId <* (char '(')) <*> call2)
        call2 = parseSpaces (callArg (Just []))

parseMainOp :: Parser Expr
parseMainOp = Parser (\str -> case str of
    [] -> Just(Nil, [])
    s -> runParser (parseAll) s
    )
    where
        parseAll = parseUnary <|> parseCall <|> id <|> wrapperParseOp
        builtIn = (parseFor <|> parseWhile <|> parseIf)
        id = parseSpaces (Id <$> parseId)

parse :: Parser Expr
parse = Parser (\str -> case str of
    [] -> Just(Nil, [])
    s -> runParser (parseAll) s
    )
    where
        parseAll = parseUnary <|> builtIn <|> definition <|> parseCall <|> wrapperParseOp
        builtIn = (parseFor <|> parseWhile <|> parseIf)
        -- id = parseSpaces (Id <$> (parseId))

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

-- readFiles :: [String] ->
