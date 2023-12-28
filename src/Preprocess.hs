module Preprocess ( preprocess ) where

import System.Environment

nextLine :: String -> (String, String)
nextLine [] = ([], [])
nextLine a = span (/='\n') a

isStmt :: String -> Bool
isStmt s = s /= [] && head s == '#'

evalStmt :: String -> IO String
evalStmt stmt = do
    if (take 8 $ dropWhile (=='\n') stmt) == "#include"
        then readFile (takeWhile (/='"') $ drop 1 $ dropWhile (/='"') stmt)
        else return ""

preprocess :: String -> IO String
preprocess str = do
    let (l, s) = nextLine str
    if isStmt l
        then (++s) <$> evalStmt l
        else return $ l ++ s