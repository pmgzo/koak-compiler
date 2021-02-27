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
parseId = Parser (\str -> runParser (parseSpaces (Wait <$> parseLetters)) str)

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
        unary = parseSpaces (Unary <$> unop *> recu)
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
    (Just (id@(Typed a b), r)) -> runParser (argArr (Just [id])) r
    )
argArr (Just array) = Parser (\str -> case runParser (parseAndWith (\_ l -> l) (char ',') parseArg) str of
    Nothing -> case runParser (char ')') str of
        Nothing -> Nothing
        (Just (')', r)) -> Just (array, r)
    (Just (id@(Typed a b), r)) -> runParser (argArr (Just (array ++ [id]))) r
    )

extractFunc :: String -> [Identifier] -> TypeKoak -> Expr -> Expr
extractFunc id args typ def = Protof (Typed id typ) args def

definition :: Parser Expr
definition = Parser (\str -> runParser def str)
    where
        def = parseSpaces (extractFunc <$> proto1 <*> (proto2 <* (char ':')) <*> typeVar <*> recu)
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

-- 7 + 3 < 4 * 5
-- (7 + 3) < (4 * 5)
--

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

parseOp :: Parser Expr
parseOp = Parser (\str -> runParser (Operation <$> parseOneOp) str)

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
        parseAll = parseUnary <|> parseCall <|> id <|> parseOp
        builtIn = (parseFor <|> parseWhile <|> parseIf)
        id = parseSpaces (Id <$> parseId)

parse :: Parser Expr
parse = Parser (\str -> case str of
    [] -> Just(Nil, [])
    s -> runParser (parseAll) s
    )
    where
        parseAll = parseUnary <|> builtIn <|> definition <|> parseCall <|> id <|> parseOp
        builtIn = (parseFor <|> parseWhile <|> parseIf)
        id = parseSpaces (Id <$> (parseId <* (char ';')))
