module Ast ( Ast (..), Op (..), addAst, astIsEmpty ) where

import Type
import Op

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast] | VarDecl Type String Bool
         | If Ast Ast Ast | Call Ast [Ast] | Init Type String Ast
         | Func Type String Ast | File [Ast] | FunDecl Type String | Literal String | Return (Maybe Ast)
         | While Ast Ast | ArrayDeref Ast Ast | For Ast Ast Ast Ast
    deriving (Show)

addAst :: Ast -> Ast -> Ast
addAst (Block list) ast = Block (ast:list)
addAst (File list) ast = File (ast:list)

astIsEmpty :: Ast -> Bool
astIsEmpty (Block list) = null list