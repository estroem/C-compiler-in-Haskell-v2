module Scope ( Scope (..), Var (..), Fun (..), Value (..),
               emptyScope, scopeAddGlo, scopeAddStc, scopeAddPar,
               scopeAddLoc, scopeAddFun, getOffset, scopeHasVar,
               scopeHasFun, scopeGetVar, scopeGetFun,
               joinScopes, hideLocals, getTotalSize ) where

import Data.List
import Data.Maybe

import Type

data Scp = Glo | Stc | Par | Loc

data Var = Var
    { varName :: String
    , varType :: Type
    , varValue :: Maybe Value
    , varIsVis :: Bool
    } deriving (Show, Eq)

data Fun = Fun
    { funName :: String
    , funRetType :: Type
    , funArgs :: [(Type, String)]
    , funIsDef :: Bool
    , numLocals :: Int
    } deriving (Show)

data Value = Integer Int | Float Float | String String
    deriving (Show, Eq)

--           Scope globals statics locals functions
data Scope = Scope [Var] [Var] [Var] [Var] [Fun]
    deriving (Show)

emptyScope :: Scope
emptyScope = (Scope [] [] [] [] [])

scopeAddGlo :: Scope -> Var -> Scope
scopeAddGlo (Scope gs ss ps ls fs) v = (Scope (v:gs) ss ps ls fs)

scopeAddStc :: Scope -> Var -> Scope
scopeAddStc (Scope gs ss ps ls fs) v = (Scope gs (v:ss) ps ls fs)

scopeAddPar :: Scope -> Var -> Scope
scopeAddPar (Scope gs ss ps ls fs) v = (Scope gs ss (v:ps) ls fs)

scopeAddLoc :: Scope -> Var -> Scope
scopeAddLoc (Scope gs ss ps ls fs) v = (Scope gs ss ps (v:ls) fs)

scopeAddFun :: Scope -> Fun -> Scope
scopeAddFun (Scope gs ss ps ls fs) f = (Scope gs ss ps ls (f:fs))

getOffset :: Scope -> String -> Maybe Int
getOffset (Scope _ _ ps ls _) n =
    let a = find (\ v -> varName v == n) ls
        in if isJust a
            then Just $ ((getLocalAddr (fromJust a) ls)
                    + roundUpTo 4 (getTypeSize $ varType $ fromJust a)) * (-1)
            else let b = find (\ v -> varName v == n) ps
                in if isJust b
                    then Just $ (getLocalAddr (fromJust b) ps) + 8
                    else Nothing

getLocalAddr :: Var -> [Var] -> Int
getLocalAddr v s =
    (foldl (\ t v -> t + roundUpTo 4 (getTypeSize $ varType v)) 0
        $ takeWhile (/=v) s)

roundUpTo :: Int -> Int -> Int
roundUpTo y x = x + y - ((x - 1) `mod` y) - 1

scopeHasVar :: Scope -> String -> Bool
scopeHasVar (Scope gs ss ps ls fs) name = any (\ v -> (varName v) == name && varIsVis v) (gs ++ ss ++ ps ++ ls)

scopeHasFun :: Scope -> String -> Bool
scopeHasFun (Scope gs ss ps ls fs) name = any (\ f -> (funName f) == name) fs

scopeGetVar :: Scope -> String -> Maybe Var
scopeGetVar (Scope gs ss ps ls fs) name = find (\ v -> (varName v) == name) (gs ++ ss ++ ps ++ ls)

scopeGetFun :: Scope -> String -> Maybe Fun
scopeGetFun (Scope gs ss ps ls fs) name = find (\ f -> (funName f) == name) fs

joinScopes :: [Scope] -> Scope
joinScopes list = joinScopesLoop list emptyScope where
    joinScopesLoop ((Scope gs ss ps ls fs):xs) (Scope rgs rss rps rls rfs) =
        joinScopesLoop xs (Scope (rgs ++ gs) (rss ++ ss) (rps ++ ps) (rls ++ ls) (rfs ++ fs))
    joinScopesLoop [] res = res

hideLocals :: Scope -> Scope
hideLocals (Scope gs ss ps ls fs) = Scope gs ss ps (map (\ (Var n t v _) -> Var n t v False) ls) fs

getTotalSize :: [Var] -> Int
getTotalSize vs = foldl (\ t v -> t + (getTypeSize $ varType v)) 0 vs