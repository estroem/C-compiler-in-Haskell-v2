module Op ( Op (..), isOperator ) where

import Data.List

data Op = Add | Sub | Mul | Div | Assign | Deref | AddrOf | Equal | NotEqual
    | LessThan | GreaterThan | LessThanEq | GreaterThanEq | LogicNot
    | PreInc | PreDec | PostInc | PostDec
    deriving (Show)

operators = ["+", "-", "*", "/", "&", "=", "==", "!=", "<", ">", "<=", ">=", "!", "++", "--", "^", "|", "<<", ">>"]

isOperator :: String -> Bool
isOperator sym = elem sym operators