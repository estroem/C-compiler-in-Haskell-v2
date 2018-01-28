module Parse.ParseDecl ( parseDecl ) where

import Data.Char
import Data.Maybe
import Data.List

import Type

parseDecl :: [String] -> (Type, String, [String])
parseDecl (x:xs) =
    let (t, n, i) = parseDecl' (x:xs) 0
        in (t, n, drop i (x:xs))

parseDecl' :: [String] -> Int -> (Type, String, Int)
parseDecl' (x:xs) s =
    if isPrimitive $ (x:xs) !! s
        then let (t, i) = parseDeclRight (x:xs) (s + 1) l r
                 n = if l == r - 2
                    then (x:xs) !! (l + 1)
                    else ""
            in (addType t $ PrimType ((x:xs) !! s), n, i - 1)
        else error "No type found"
    where
        (l, r) = findDeclStart (x:xs) (s + 1)

parseDeclLeft :: [String] -> Int -> Int -> Int -> (Type, Int)
parseDeclLeft (x:xs) start l r
    | l < start = (EmptyType, r)
    | (x:xs) !! l == "*" = let (a, b) = parseDeclLeft (x:xs) start (l - 1) r
                               in (PtrType a, b)
    | (x:xs) !! l == "(" = parseDeclRight (x:xs) start (l - 1) r

parseDeclRight :: [String] -> Int -> Int -> Int -> (Type, Int)
parseDeclRight (x:xs) start l r
    | (x:xs) !! r == "(" =
        let (args, r') = parseDeclArgs (x:xs) (r + 1)
            (ret, end) = parseDeclRight (x:xs) start l r'
            in (FuncType ret args, end)
    | (x:xs) !! r == "[" =
        let (arrSize, r') = parseDeclArr (x:xs) (r + 1)
            (arrType, end) = parseDeclRight (x:xs) start l r'
            in (ArrayType arrType arrSize, end)
    | otherwise = parseDeclLeft (x:xs) start l (r + 1)

parseDeclArr :: [String] -> Int -> (Int, Int)
parseDeclArr (x:xs) i
    | (x:xs) !! i == "]" = (0, i + 1)
    | all isDigit $ (x:xs) !! i = (read $ (x:xs) !! i, i + 2)

parseDeclArgs :: [String] -> Int -> ([(Type, String)], Int)
parseDeclArgs (x:xs) i
    | (x:xs) !! i == ")" = ([], i + 1)
    | (x:xs) !! i' == "," = let (ts, i'') = parseDeclArgs (x:xs) (i' + 1)
                                in ((t, n):ts, i'')
    | otherwise = ([(t, n)], i' + 1)
    where
        (t, n, i') = parseDecl' (x:xs) i

findDeclStart :: [String] -> Int -> (Int, Int)
findDeclStart l s =
    if isJust j
        then if isJust i && fromJust i < fromJust j
            then let i' = fromJust i in (s + i' - 1, s + i' + 1)
            else let j' = fromJust j in (s + j' - 1, s + j')
        else error "No end of statement found"
    where
        i = findIndex isValidVar $ drop s l
        j = findIndex (\ s -> isClosingSym s || s == "[") $ drop s l

addType :: Type -> Type -> Type
addType (PrimType _) _ = error "Cannot add to primitive type"
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)
addType (ArrayType a i) b = (ArrayType (addType a b) i)

getMod :: [String] -> (Maybe String, [String])
getMod (x:xs)
    | x == "static" || x == "extern" = (Just x, xs)
    | otherwise = (Nothing, (x:xs))

isClosingSym :: String -> Bool
isClosingSym = (flip elem) [";", ",", ")", "]"]

isValidVar :: String -> Bool
isValidVar = all isAlpha