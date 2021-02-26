module Parse2 where

import DataType2
import MyParser
import Control.Applicative


computeMultipleParse :: [Parser String] -> Parser String
computeMultipleParse [a]        = a
computeMultipleParse (a:rest)   = Parser $lbd (parseAnd a (computeMultipleParse rest))
                                where 
                                lbd = (\fct ->  
                                        \str -> case (runParser fct str) of 
                                                (Just ((s1, s2), rest)) -> (Just ((s1 ++ s2), rest))
                                                Nothing -> Nothing)

parseNum :: Parser String
parseNum = d
            where
            d = computeMultipleParse parseDArray
            wrapperParseChar = (\psr -> \str -> 
                                        case (runParser psr str) of
                                        (Just (a, rest)) -> (Just ([a], rest))
                                        _ -> Nothing)
            parseDArray = [
                            (parseMaybeChar '-'),
                            (parseSome (parseAnyChar "0123456789")),
                            (Parser (wrapperParseChar (parseChar '.'))),
                            (parseSome (parseAnyChar "0123456789")) ]

parseMaybeChar :: Char -> Parser String
parseMaybeChar c = Parser $lbd (parseChar c) --parseMany (parseAnyChar str)
                where
                lbd = (\psr -> 
                        \str -> case (runParser psr str) of
                                    Nothing -> Just ("", str)
                                    Just (c, str) -> Just ([c], str) )

parseDouble2 :: Parser Double
parseDouble2 = Parser (lbd parseNum)
            where
            lbd = (\psr -> \str -> case (runParser parseNum str) of 
                                    Just (str, rest) -> (Just (read str :: Double, rest))
                                    Nothing       -> Nothing
                                    )

-- a -> b -> c -> d
