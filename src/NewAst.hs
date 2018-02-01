module NewAst ( File (..), Symb (..), Stmt (..), Expr (..) ) where

import Type

data File = File [Symb] deriving (Show)
data Symb = FunDecl Type String | Func Type String [Stmt] | VarDecl Type String Bool | Init Type String Expr deriving (Show)
data Stmt = If Expr Stmt Stmt | Nop | Expr Expr | Block [Stmt] | LocVar Type String Bool (Maybe Expr) deriving (Show)
data Expr = Number Integer | Name String | App String [Expr] | Call Expr [Expr] | Literal String deriving (Show)