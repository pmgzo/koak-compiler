import System.Exit (exitSuccess, exitWith)
import System.Exit
import LLVM_Module
import TypeInference
import DataType2
import Parse
import MyParser
import System.Directory

import System.Environment
import System.Exit (exitSuccess, exitWith)
import System.Exit
import System.IO
import Control.Exception
import Data.Maybe

import Error

mergeLine :: Maybe ([Expr]) -> Maybe (Expr, String) -> Maybe ([Expr])
mergeLine (Just e) (Just (e1, s)) = Just ([e1] ++ e)
mergeLine Nothing (Just (e1, s))  = Just ([e1])
mergeLine (Just e) Nothing        = Nothing
mergeLine _ _                     = Nothing

getStatement :: String -> String -> Maybe ([Expr])
-- getStatement [] statement       = mergeLine (Just [(Err statement)]) (runParser parse statement)
getStatement [] statement       = mergeLine (Just []) (runParser parse statement)
getStatement (';':xs) statement = mergeLine (getStatement xs "") (runParser parse (statement ++ ";"))
getStatement (x:xs) statement   = getStatement xs (statement ++ [x])


parseFile :: String -> Maybe [Expr]
parseFile [] = Just []
-- parseFile str = Just [(Err str)]
parseFile content
          | res == Nothing = Nothing
          | otherwise      = res
          where res = getStatement content ""

mergeMaybe :: Maybe [Expr] -> [[Expr]] -> [[Expr]]
mergeMaybe Nothing _   = []
mergeMaybe (Just []) _ = []
mergeMaybe _ []        = []
mergeMaybe (Just a) b  = a:b

checkNothing :: Maybe [Expr] -> [[Expr]]
checkNothing Nothing   = []
checkNothing (Just []) = []
checkNothing (Just a)  = [a]

parseFiles :: [String] -> [[Expr]]
parseFiles [] = []
parseFiles (file:[]) = checkNothing $ parseFile file
parseFiles (file:xs) = mergeMaybe (parseFile file) (parseFiles xs)


getContent :: [String] -> IO (Maybe [String])
getContent [] = return (Just [])
getContent (file:r) = do
    b <- doesFileExist file
    case b of
        False -> return Nothing
        True -> do
            -- f <- readFile file
            handle <- openFile file ReadMode
        
            hSetEncoding handle latin1
            f <- hGetContents handle

            rest <- getContent r
            case rest of
                Just list -> return $ Just (f:list)
                Nothing -> return Nothing

getErr :: [Expr] -> String
getErr ((Err str):xs) = str ++ (getErr xs)
getErr _ = ""
-- genErr _ = "" ++ (getErr xs)

genFile :: [(String, [Expr])] -> IO ()
genFile [] = return ()
genFile ((filename,expr):xs)
        | res == []  = print $ show expr -- print "empty array" >> exitWith (ExitFailure 84)
        | err /= ""  = print err >> exitWith (ExitFailure 84)
        | err2 /= "" = print err2 >> exitWith (ExitFailure 84)
        | otherwise  = genObjFromExpr filename res >> genFile xs
        where res = inferringType expr
              err = getErr res
              err2 = getErr $findTrickyError res

main :: IO()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "The REPL is not implemented" >>
            exitWith (ExitFailure 84)
        _ -> do
            print (show args)
            contents <- getContent args
            -- print (len contents)
            case contents of
                Nothing -> hPutStrLn stderr "file not found" >> exitWith (ExitFailure 84)
                Just cont -> case parseFiles cont of
                    [] -> hPutStrLn stderr "The files are invalid" >> exitWith (ExitFailure 84)
                    expr -> genFile (zip args expr)