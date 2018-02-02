module Type ( Type (..), getIntType, canCast, getType, getTypeSize, isPrimitive, getPtrType, addType ) where

import Data.Maybe
import Data.List

data Type = PrimType String | PtrType Type | FuncType Type [(Type, String)] | ArrayType Type Int | EmptyType

instance Eq Type where
    (PtrType a) == (PtrType b) = a == b
    (PrimType a) == (PrimType b) = a == b
    (FuncType a b) == (FuncType c d) = a == c && (fst $ unzip b) == (fst $ unzip d)
    (ArrayType a i) == (ArrayType b j) = a == b && i == j
    EmptyType == EmptyType = True
    _ == _ = False

instance Show Type where
    show t = showType t ""


prims = [("int", 4), ("short", 2), ("char", 1), ("float", 4), ("double", 8)]


getIntType :: Integer -> Maybe Type
getIntType i =
    if snd t /= 0
        then Just $ PrimType $ fst $ t
        else Nothing
    where t = foldl (\ a b -> if ((snd a) == 0 || (snd b) < (snd a)) && 2^(snd b * 8) `div` 2 > i then b else a) ("", 0) prims

canCast :: Type -> Type -> Bool
canCast (PrimType l) (PrimType r)
    | l == r = True
    | typeIsFloat l && typeIsFloat r = (typeSize l) > (typeSize r)
    | not (typeIsFloat l) && typeIsFloat r = False
    | typeIsFloat l && not (typeIsFloat r) = True
    | otherwise = (typeSize l) > (typeSize r)
canCast l r = l == r

getType :: String -> Type -> Type -> Maybe Type
getType sym l r = case sym of
    "+" -> getAddType l r
    "-" -> getAddType l r
    "*" -> getMulType l r
    "/" -> getMulType l r
    "=" -> Just l
    "$" -> case l of
        (PtrType t) -> Just t
        (ArrayType t _) -> Just t
        _ -> error "Can not dereference non-pointer type"
    "&" -> Just $ PtrType l
    "!=" -> Just $ PrimType "int"

getMulType :: Type -> Type -> Maybe Type
getMulType (PtrType _) _ = Nothing
getMulType _ (PtrType _) = Nothing
getMulType (ArrayType _ _) _ = Nothing
getMulType _ (ArrayType _ _) = Nothing
getMulType l r = getAddType l r

getAddType :: Type -> Type -> Maybe Type
getAddType (PrimType l) (PrimType r)
    | l == r = Just $ PrimType l
    | typeIsFloat l && typeIsFloat r = Just $ PrimType $ biggestType l r
    | not (typeIsFloat l) && typeIsFloat r = Just $ PrimType r
    | typeIsFloat l && not (typeIsFloat r) = Just $ PrimType l
    | otherwise = Just $ PrimType $ biggestType l r
getAddType (PtrType _) (PtrType _) = Nothing
getAddType l@(PtrType _) (PrimType r)
    | typeIsFloat r = Nothing
    | otherwise = Just l
getAddType (PrimType l) r@(PtrType _)
    | typeIsFloat l = Nothing
    | otherwise = Just r
getAddType l@(ArrayType _ _) (PrimType r)
    | typeIsFloat r = Nothing
    | otherwise = Just l
getAddType (PrimType l) r@(ArrayType _ _)
    | typeIsFloat l = Nothing
    | otherwise = Just r
getAddType _ _ = Nothing

typeIsFloat :: String -> Bool
typeIsFloat t = if t == "float" || t == "double" then True else False

biggestType :: String -> String -> String
biggestType a b = if (f a) >= (f b) then a else b
    where
        f = \ x -> snd $ fromJust $ find ((==x) . fst) prims

typeSize :: String -> Integer
typeSize t = snd $ fromJust $ find ((==t) . fst) prims

showType :: Type -> String -> String
showType (PrimType t) str = t ++ str
showType (PtrType t) str = showType t $ "*" ++ str
showType (FuncType ret args) str =
    showType ret $ "(" ++ str ++ ")(" ++
                   (intercalate ", " $
                        map ((flip showType) "") $ fst $ unzip args) ++ ")"
showType (ArrayType t i) str = showType t $ str ++ "[" ++ show i ++ "]"
showType EmptyType _ = "EmptyType"

getTypeSize :: Type -> Int
getTypeSize (ArrayType t i) = i * getTypeSize t
getTypeSize (PtrType _) = 4
getTypeSize (PrimType "int") = 4
getTypeSize (PrimType "short") = 2
getTypeSize (PrimType "byte") = 1
getTypeSize (PrimType "char") = 1

isPrimitive :: String -> Bool
isPrimitive str = elem str $ fst $ unzip prims

getPtrType :: Type -> Maybe Type
getPtrType (PtrType t) = Just t
getPtrType (ArrayType t _) = Just t
getPtrType _ = Nothing

addType :: Type -> Type -> Type
addType (PrimType _) _ = error "Cannot add to primitive type"
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)
addType (ArrayType a i) b = (ArrayType (addType a b) i)