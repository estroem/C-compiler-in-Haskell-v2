module NewAst ( File (..), Symb (..), Stmt (..), Expr (..) ) where

import Type

data File = File [Symb]
data Symb = FunDecl Type String | Func Type String [Stmt] | VarDecl Type String Bool | Init Type String Expr
data Stmt = If Expr Stmt Stmt | Nop | Expr Expr | Block [Stmt] | LocVar Type String Bool
data Expr = Number Integer | Name String | App String [Expr] | Call Expr [Expr] | Literal String