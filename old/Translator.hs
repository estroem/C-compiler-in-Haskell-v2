module Translator ( translate ) where

import Ast
import NewAst
import Op

translate :: Ast -> File
translate (Ast.File l) = NewAst.File $ map translateSymb l

translateSymb :: Ast -> Symb
translateSymb (Ast.VarDecl t s b) = NewAst.VarDecl t s b
translateSymb (Ast.Init t s e) = NewAst.Init t s $ translateExpr e
translateSymb (Ast.FunDecl t s) = NewAst.FunDecl t s
translateSymb (Ast.Func t s (Ast.Block l)) = NewAst.Func t s $ map translateStmt l

translateStmt :: Ast -> Stmt
translateStmt (Ast.If a b c) = NewAst.If (translateExpr a) (translateStmt b) (translateStmt c)
translateStmt (Ast.Block x) = NewAst.Block $ map translateStmt x
translateStmt (Ast.VarDecl t s b) = NewAst.LocVar t s b Nothing
translateStmt (Ast.Init t s a) = NewAst.LocVar t s False $ Just $ translateExpr a
translateStmt a = NewAst.Expr $ translateExpr a

translateExpr :: Ast -> Expr
translateExpr (Ast.Number x) = NewAst.Number x
translateExpr (Ast.Name s) = NewAst.Name s
translateExpr (Ast.App o e) = NewAst.App (symbol o) $ map translateExpr e
translateExpr (Ast.Call a b) = NewAst.Call (translateExpr a) $ map translateExpr b
translateExpr (Ast.Literal a) = NewAst.Literal a
translateExpr a = error  $ show a