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



mergeLine :: Maybe ([Expr]) -> Maybe (Expr, String) -> Maybe ([Expr])
mergeLine (Just e) (Just (e1, s)) = Just ([e1] ++ e)
mergeLine Nothing (Just (e1, s))  = Just ([e1])
mergeLine (Just e) Nothing        = Just (e)
mergeLine _ _                     = Nothing

getStatement :: String -> String -> Maybe ([Expr])
getStatement [] statement       = mergeLine (Just []) (runParser parse statement)
getStatement (';':xs) statement = mergeLine (getStatement xs "") (runParser parse statement)
getStatement (x:xs) statement   = getStatement xs (statement ++ [x])


parseFile :: String -> Maybe [Expr]
parseFile [] = Just []
parseFile content
          | res == Nothing = Nothing
          | otherwise      = res
          where res = getStatement content ""

mergeMaybe :: Maybe a -> [a] -> [a]
mergeMaybe Nothing _ = []
mergeMaybe (Just a) b = a:b

parseFiles :: [String] -> [[Expr]]
parseFiles [] = []
parseFiles (file:xs) = mergeMaybe (parseFile file) (parseFiles xs)


getContent :: [String] -> IO (Maybe [String])
getContent [] = return (Just [])
getContent (file:r) = do
    b <- doesFileExist file
    case b of
        False -> return Nothing
        True -> do
            f <- readFile file
            rest <- getContent r
            case rest of
                Just list -> return $ Just (f:list)
                Nothing -> return Nothing

getErr :: [Expr] -> String
getErr ((Err str):xs) = str ++ (getErr xs)
getErr _ = ""
-- genErr _ = "" ++ (getErr xs)

genFile :: [Expr] -> IO ()
genFile expr
        | res == [] = print "empty array" >> exitWith (ExitFailure 84)
        | err /= "" = print err >> exitWith (ExitFailure 84)
        | otherwise = genObjFromExpr "obj" res
        where res = inferringType expr
              err = getErr res

main :: IO()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "The REPL is not implemented" >>
            exitWith (ExitFailure 84)
        _ -> do
            contents <- getContent args
            case contents of
                Nothing -> hPutStrLn stderr "file not found" >> exitWith (ExitFailure 84)
                Just cont -> case parseFiles cont of
                    [] -> print "The files are invalid" >> exitWith (ExitFailure 84)
                    expr -> (map genFile expr)!!0
                    -- expr -> case inferringType (expr!!0) of
                    --     [(Err str)] -> print(str) >> exitWith (ExitFailure 84)
                    --     [] -> print "empty array" >> exitWith (ExitFailure 84)
                    --     expr1 -> genObjFromExpr "obj" expr1


-- main :: IO ()
-- main = do
--   let exprs = inferringType []--functionPaul
--   case exprs of
--     [(Err str)] -> exitWith (ExitFailure 84)
--     []  -> print("empty array") >> exitWith (ExitFailure 84)
--     exprs2 -> genObjFromExpr "obj" exprs2
