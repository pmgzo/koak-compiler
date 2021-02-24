-- 
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-paul.cochet
-- File description:
-- MyParser.hs
--

module MyParser where

import Control.Applicative
import Data.List

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f parser = Parser (
        \ str -> case runParser parser str of
            Nothing -> Nothing
            Just (a, rest) -> Just (f a, rest)
        )

instance Applicative Parser where
    pure x = Parser $ \ str -> Just (x, str)
    a <*> b = Parser (
        \ str -> case runParser a str of
            Just (fct, rest) -> case runParser b rest of
                Nothing -> Nothing
                Just (r2, end) -> Just ((fct $ r2), end)
            Nothing -> Nothing
        )

instance Alternative Parser where
    empty = Parser (\ _ -> Nothing)
    a <|> b = Parser (
        \ str -> case runParser a str of
            Nothing -> runParser b str
            r -> r
        )

parseChar :: Char -> Parser Char
parseChar c = Parser (
    \ str -> case str of
        (a:str) | (a == c) -> Just (c, str)
        _ -> Nothing 
    )

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = Parser (
    \ str -> case runParser a str of
        Nothing -> runParser b str
        r -> r
    )

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser ( \ _ -> Nothing)
parseAnyChar (a:filter) = Parser (
    \ str -> runParser (parseOr (parseChar a) (parseAnyChar filter)) str
    )

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b = Parser (
    \ str -> case runParser a str of
        Just (r1, rest) -> case runParser b rest of
            Nothing -> Nothing
            Just (r2, end) -> Just ((r1, r2), end)
        Nothing -> Nothing
    )

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fn a b = Parser (
    \ str -> case runParser a str of
        Just (r1, rest) -> case runParser b rest of
            Nothing -> Nothing
            Just (r2, end) -> Just (fn r1 r2, end)
        Nothing -> Nothing
    )

parseMany :: Parser a -> Parser [a]
parseMany a = Parser (
    \ str -> case runParser a str of
        Just (one, rest) -> case runParser (parseMany a) rest of
            Just (arr, end) -> Just (one:arr, end)
        Nothing -> Just ([], str)
    )

parseSome :: Parser a -> Parser [a]
parseSome a = Parser (
    \ str -> runParser (parseAndWith (\ x y -> x:y) a (parseMany a)) str
    )

parseSpaces :: Parser a -> Parser a
parseSpaces a = Parser (
    \str -> runParser (spaces *> a) str
    )
    where spaces = (parseMany (parseChar ' '))

parseSomeSpaces :: Parser a -> Parser a
parseSomeSpaces a = Parser (
    \str -> runParser (spaces *> a) str
    )
    where spaces = (parseSome (parseChar ' '))


parseUInt :: Parser Int
parseUInt = let
    readInt = (\ a -> read a::Int)
    in Parser (
    \ str -> runParser (readInt <$> (parseSome (parseAnyChar "0123456789"))) str
    )

parseInt :: Parser Int
parseInt = Parser (
    \ s -> case s of
        ('+':str) -> runParser parseUInt str
        ('-':str) -> runParser ((\ x -> (-1) * x) <$> parseUInt) str
        str -> runParser parseUInt str
    )

parseUInte :: Parser Integer
parseUInte = let
    readInte = (\ a -> read a::Integer)
    in Parser (
    \ str -> runParser (readInte <$> (parseSome (parseAnyChar "0123456789"))) str
    )

parseInte :: Parser Integer
parseInte = Parser (
    \ s -> case s of
        ('+':str) -> runParser parseUInte str
        ('-':str) -> runParser ((\ x -> (-1) * x) <$> parseUInte) str
        str -> runParser parseUInte str
    )


parseAFloat :: Parser Float
parseAFloat = let
    readFloat = (\ a -> read a::Float)
    glue = parseAndWith (\ x y -> x:y)
    glue2 = parseAndWith (\ x y -> x ++ y)
    parseDigit = (parseAnyChar "0123456789")
    parseAnInt = (parseMany parseDigit)
    pointArr = ((\ a -> [a]) <$> parseChar '.')
    parseWFloat = (glue2 (glue2 parseAnInt pointArr) (parseMany parseDigit))
    iI = (\ a -> fromIntegral a::Float) <$> parseInt
    in Parser (\ str -> runParser ((readFloat <$> parseWFloat) <|> (iI)) str)

parseFloat :: Parser Float
parseFloat = Parser (
    \ s -> case last s of
        '.' -> Nothing
        _ -> case s of
            ('+':str) -> runParser parseAFloat str
            ('-':str) -> runParser ((\ x -> (-1) * x) <$> parseAFloat) str
            str -> runParser parseAFloat str
    )

parseADouble :: Parser Double
parseADouble = let
    readDouble = (\ a -> read a::Double)
    glue = parseAndWith (\ x y -> x:y)
    glue2 = parseAndWith (\ x y -> x ++ y)
    parseDigit = (parseAnyChar "0123456789")
    parseAnInt = (parseMany parseDigit)
    pointArr = ((\ a -> [a]) <$> parseChar '.')
    parseWDouble = (glue2 (glue2 parseAnInt pointArr) (parseMany parseDigit))
    iI = (\ a -> fromIntegral a::Double) <$> parseInt
    in Parser (\ str -> runParser ((readDouble <$> parseWDouble) <|> (iI)) str)

parseDouble :: Parser Double
parseDouble = Parser (
    \ s -> case last s of
        '.' -> Nothing
        _ -> case s of
            ('+':str) -> runParser parseADouble str
            ('-':str) -> runParser ((\ x -> (-1) * x) <$> parseADouble) str
            str -> runParser parseADouble str
    )

parseLetters :: Parser String
parseLetters = let
    parseOneLetter = (parseAnyChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    parseRest = (parseMany parseOneLetter)
    in Parser (
    \ str -> runParser (parseAndWith (\x y -> x:y) parseOneLetter parseRest) str
    )

parseWord :: String -> Parser String
parseWord word = let
    in Parser (
    \ str -> case runParser parseLetters str of
        Just(word, r) -> Just(word, r)
        _ -> Nothing
    )

parseAnyStr :: [String] -> Parser String
parseAnyStr (a:r) = Parser (\str -> runParser parseOneStr str)
    where
        parseOneStr = (parseWord a) <|> (parseAnyStr r)

parseSign :: Parser Char
parseSign = Parser (
    \ str -> case str of
        [] -> Nothing
        ('^':r) -> Just ('^', r)
        ('/':r) -> Just ('/', r)
        ('*':r) -> Just ('*', r)
        ('-':r) -> Just ('-', r)
        ('+':r) -> Just ('+', r)
        _ -> Nothing
    )

parseTuple :: Parser a -> Parser (a, a)
parseTuple parse = let
    keepSec = parseAndWith (\ x y -> y)
    in Parser (
    \ str -> case runParser (keepSec (parseChar '(') parse) str of
        Nothing -> Nothing
        Just (first, r1) -> case runParser (keepSec (parseChar ',') parse) r1 of
            Nothing -> Nothing
            Just (second, r2) -> case runParser (parseChar ')') r2 of
                Nothing -> Nothing
                Just (_, rest) -> Just ((first, second), rest)
    )