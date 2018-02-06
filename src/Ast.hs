module Ast ( File (..), Symb (..), Stmt (..), Expr (..) ) where

import Type

data File = File [Symb] deriving (Show)
data Symb = FunDecl Type String | Func Type String [Stmt] | VarDecl Type String Bool | Init Type String Expr deriving (Show)
data Stmt = If Expr Stmt Stmt | While Expr Stmt | For Stmt Expr Expr Stmt | Nop | Expr Expr | Block [Stmt]
    | LocVar Type String Bool (Maybe Expr) | Return (Maybe Expr) | Break | Continue | Goto String | GotoLabel String deriving (Show)
data Expr = Number Integer | Name String | App String [Expr] | Call Expr [Expr] | Literal String
    | ArrayDeref Expr Expr | Ternary Expr Expr Expr deriving (Show)