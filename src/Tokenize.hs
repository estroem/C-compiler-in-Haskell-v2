module Tokenize ( tokenize ) where

import Data.Char

import Op

extraSymbols = [";", "(", ")", "{", "}", ",", "[", "]", "?", ":", "'"]

tokenize :: String -> [String]
tokenize [] = []
tokenize ('/':'/':xs) = tokenize $ dropWhile (/= '\n') xs
tokenize (x:xs)
    | isDigit x || x == '.' =
        if (head $ dropWhile isDigit xs) == '.'
            then let (num, r) = span isDigit (x:xs)
                     (dec, r') = span isDigit (tail r)
               in (num ++ "." ++ dec) : tokenize r'
            else (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
    | x == '"' = (x : takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs)
    | x == ' ' || x == '\t' || x == '\n' || x == '\r' = tokenize xs
    | length xs >= 2 && symExists (x:take 2 xs) = (x : (take 2 xs)) : (tokenize (drop 2 xs))
    | length xs >= 1 && symExists (x:take 1 xs) = (x : (take 1 xs)) : (tokenize (tail xs))
    | symExists [x] = [x] : tokenize xs
    | otherwise = error $ "Illegal symbol \"" ++ [x] ++ "\"" ++ (show xs)

symExists :: String -> Bool
symExists sym = isOperator sym || elem sym extraSymbols