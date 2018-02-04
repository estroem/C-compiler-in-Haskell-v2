module Rtl ( Rtl, RtlLine (..), Lit, Reg, reg_ebp, reg_esp, reg_eax, toRtl ) where

import Data.Maybe

import Parse
import Scope
import Type
import Id
import Op
import Ast

type Lit = String

data RtlLine = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer
             | Load Reg String | Save String Reg Integer | SaveToPtr Reg Reg Integer | Label String
             | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
             | CallName String [Reg] Reg | CallAddr Reg [Reg] Reg | DeRef Reg
             | FuncStart String | FuncEnd String | Ret String | Push Reg | LoadLoc Reg Integer
             | SaveLoc Integer Reg | Pop Reg | AddConst Reg Integer | SubConst Reg Integer
             | MulConst Reg Integer | DivConst Reg Integer | LoadLit Reg Lit | MovReg Reg Reg
             | Addr Reg String | AddrLoc Reg Integer | Test Reg | Setz Reg | Setl Reg | Setg Reg
             | Setle Reg | Setge Reg | AndConst Reg Integer
    deriving (Show)
type Reg = Integer

reg_ebp :: Integer
reg_ebp = -2
reg_esp :: Integer
reg_esp = -1
reg_eax :: Integer
reg_eax = -3

type Rtl = [RtlLine]


toRtl :: Ast -> (Rtl, Scope)
toRtl tree = fileToRtl tree emptyScope

fileToRtl :: Ast -> Scope -> (Rtl, Scope)
fileToRtl (File []) _ = ([], emptyScope)
fileToRtl (File [x]) scope = entityToRtl x scope
fileToRtl (File (x:xs)) scope = (expr ++ file, joinScopes [scope1, scope2])
    where
        (expr, scope1) = entityToRtl x scope
        (file, scope2) = fileToRtl (File xs) (joinScopes [scope, scope1])


entityToRtl :: Ast -> Scope -> (Rtl, Scope)

entityToRtl (VarDecl t n s) scope = ([], scopeAddGlo emptyScope (Var n t Nothing True))

entityToRtl (Init t n v) _ = ([], scopeAddGlo emptyScope $ Var n t (getValueFromAst v) True)

entityToRtl (FunDecl (FuncType retType argTypes) name) scope = ([], scopeAddFun emptyScope (Fun name retType argTypes False 0))

entityToRtl (Func (FuncType retType argTypes) name body) scope = ([Label ('_':name), Push reg_ebp, MovReg reg_ebp reg_esp] ++
                                                                    (if localSize > 0
                                                                        then [SubConst reg_esp localSize]
                                                                        else []) ++
                                                                    bodyRtl ++
                                                                    (if endsOnRet body
                                                                        then []
                                                                        else [Ret name]),
                                                                    (Scope [] ss [] [] [thisFun]))
    where
        (bodyRtl, (Scope _ ss _ ls _)) = blockToRtl body (joinScopes [(Scope [] [] (argTypesToVars argTypes) [] [thisFun]), scope]) (funcId name)
        localSize = getTotalSize ls
        thisFun = Fun name retType argTypes True localSize


blockToRtl :: Ast -> Scope -> Id -> (Rtl, Scope)
blockToRtl (Block []) scope _ = ([], emptyScope)
blockToRtl (Block [x]) scope id = lineToRtl x scope id
blockToRtl (Block (x:xs)) scope id = (expr ++ block, hideLocals $ joinScopes [scope1, scope2])
    where
        (expr, scope1) = lineToRtl x scope id
        (block, scope2) = blockToRtl (Block xs) (joinScopes [scope, scope1]) (incId id)


lineToRtl :: Ast -> Scope -> Id -> (Rtl, Scope)

lineToRtl (If cond thenBlock elseBlock) scope id
    | not $ astIsEmpty elseBlock = (condRtl ++
                                 [Cmp condReg, Jne ((getIdString newId) ++ "then")] ++
                                 elseBlockRtl ++
                                 [Jmp ((getIdString newId) ++ "end"),
                                 Label ((getIdString newId) ++ "then")] ++
                                 thenBlockRtl ++
                                 [Label ((getIdString newId) ++ "end")],
                                 joinScopes [thenNewVars, elseNewVars])
    | otherwise = (condRtl ++ [Cmp condReg, Je ((getIdString newId) ++ "end")] ++ thenBlockRtl ++ [Label ((getIdString newId) ++ "end")], thenNewVars)
    where
        (condRtl, condReg, _) = exprToRtl cond 0 scope
        (thenBlockRtl, thenNewVars) = blockToRtl thenBlock scope newId
        (elseBlockRtl, elseNewVars) = blockToRtl elseBlock scope newId
        newId = addIfId id

lineToRtl (While cond block) scope id = ([Label ((getIdString newId) ++ "while")] ++
                                         condRtl ++
                                         [Cmp condReg, Je ((getIdString newId) ++ "end")] ++
                                         blockRtl ++
                                         [Jmp ((getIdString newId) ++ "while"), Label ((getIdString newId) ++ "end")],
                                         newVars)
    where
        (condRtl, condReg, _) = exprToRtl cond 0 scope
        (blockRtl, newVars) = blockToRtl block scope newId
        newId = addLoopId id

lineToRtl (For init cond inc block) scope id = (initRtl ++
                                         [Label ((getIdString newId) ++ "for")] ++
                                         condRtl ++
                                         [Cmp condReg, Je ((getIdString newId) ++ "end")] ++
                                         blockRtl ++
                                         incRtl ++
                                         [Jmp ((getIdString newId) ++ "for"), Label ((getIdString newId) ++ "end")],
                                         joinScopes [hideLocals initVar, newVars])
    where
        (initRtl, initVar) = lineToRtl init scope id
        scope' = joinScopes [scope, initVar]
        (condRtl, condReg, _) = exprToRtl cond 0 scope'
        (incRtl, _, _) = exprToRtl inc 0 scope'
        (blockRtl, newVars) = blockToRtl block scope' newId
        newId = addLoopId id

lineToRtl (Return Nothing) _ id = ([Ret $ getFuncId id], emptyScope)

lineToRtl (Return (Just expr)) scope id =
    if canCast retType (fromJust exprType)
        then (exprRtl ++ [MovReg reg_eax reg, Ret $ getFuncId id], emptyScope)
        else error $ "Cannot autocast " ++ (show $ fromJust exprType) ++ " to " ++ (show retType)
    where
        (exprRtl, reg, exprType) = exprToRtl expr 0 scope
        retType = funRetType $ fromJust $ scopeGetFun scope $ getFuncId id

lineToRtl (VarDecl t n s) _ _ = ([], (if s then scopeAddStc else scopeAddLoc) emptyScope (Var n t Nothing True))

lineToRtl (Init t n v) scope _ = (rtl, scope')
    where
        (rtl, _, _) = exprToRtl (App (getOpFromSym "=") [Name n, v]) 0 scope'
        scope' = scopeAddLoc scope (Var n t Nothing True)

lineToRtl a b _ = let (c, _, _) = exprToRtl a 0 b in (c, emptyScope)


exprToRtl :: Ast -> Reg -> Scope -> (Rtl, Reg, Maybe Type)

exprToRtl (Number x) nextReg scope = ([Mov (nextReg + 1) x], nextReg + 1, getIntType x)

exprToRtl (Literal l) nextReg _ = ([LoadLit (nextReg + 1) l], nextReg + 1, Just $ PtrType $ PrimType "char")

exprToRtl (Name name) nextReg scope =
    if scopeHasVar scope name
        then let i = getOffset scope name
            in case fromJust varTyp of
                (ArrayType t _) -> if isJust i
                    then if fromJust i > 0
                        then ([LoadLoc (nextReg + 1) (fromJust i)], nextReg + 1, varTyp)
                        else ([AddrLoc (nextReg + 1) (fromJust i)], nextReg + 1, varTyp)
                    else ([Addr (nextReg + 1) name], nextReg + 1, varTyp)
                _ -> if isJust i
                    then ([LoadLoc (nextReg + 1) (fromJust i)], nextReg + 1, varTyp)
                    else ([Load (nextReg + 1) name], nextReg + 1, varTyp)
        else if scopeHasFun scope name
            then ([Load (nextReg + 1) name], nextReg + 1, funTyp)
            else error $ "Variable not in scope: " ++ name
    where
        varTyp = Just $ varType $ fromJust $ scopeGetVar scope name
        funTyp = Just $ FuncType (funRetType fun) (funArgs fun)
        fun = fromJust $ scopeGetFun scope name

exprToRtl (App op@(Op {symbol="+"}) [expr1, expr2]) nextReg scope =
    (expr2' ++ expr1' ++
        fixPtrOffset reg1 reg2 (fromJust lType) ++
        fixPtrOffset reg2 reg2 (fromJust rType) ++
        [Add reg2 reg1],
    reg2, getType (symbol op) (fromJust lType) (fromJust rType))
    where
        (expr2', reg1, rType) = exprToRtl expr2 nextReg scope
        (expr1', reg2, lType) = exprToRtl expr1 reg1 scope

exprToRtl (App op@(Op {symbol="-"}) [expr1, expr2]) nextReg scope =
    (expr2' ++ expr1' ++
        fixPtrOffset reg1 reg2 (fromJust lType) ++
        fixPtrOffset reg2 reg2 (fromJust rType) ++
        [Sub reg2 reg1],
    reg2, getType (symbol op) (fromJust lType) (fromJust rType))
    where
        (expr2', reg1, rType) = exprToRtl expr2 nextReg scope
        (expr1', reg2, lType) = exprToRtl expr1 reg1 scope
        
exprToRtl (App op exprList) nextReg scope =
    (fst a, snd a, if (numArgs op) == 1
                       then getType (symbol op) (fromJust typ) undefined
                       else getType (symbol op) (fromJust lType) (fromJust rType))
    where
        (expr1, reg1, rType) = exprToRtl (exprList !! 1) nextReg scope
        (expr2, reg2, lType) = exprToRtl (exprList !! 0) reg1 scope
        (expr, reg, typ) = exprToRtl (head exprList) nextReg scope
        a = case symbol op of
            "*" -> (expr1 ++ expr2 ++ [Mul reg2 reg1], reg2)
            "/" -> (expr1 ++ expr2 ++ [Div reg2 reg1], reg2)
            "=" -> handleAssign (head exprList) (last exprList) nextReg scope
            "$" -> (expr ++ [DeRef reg], reg)
            "==" -> (expr1 ++ expr2 ++ [Sub reg2 reg1, Setz reg2, AndConst reg2 1], reg2)
            "!=" -> (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2)
            "<" -> (expr1 ++ expr2 ++ [Sub reg2 reg1, Setl reg2, AndConst reg2 1], reg2)
            ">" -> (expr1 ++ expr2 ++ [Sub reg2 reg1, Setg reg2, AndConst reg2 1], reg2)
            "<=" -> (expr1 ++ expr2 ++ [Sub reg2 reg1, Setle reg2, AndConst reg2 1], reg2)
            ">=" -> (expr1 ++ expr2 ++ [Sub reg2 reg1, Setge reg2, AndConst reg2 1], reg2)
            "&" -> handleAddr (head exprList) nextReg scope
            "!" -> (expr ++ [Test reg, Setz reg, AndConst reg 1], reg)

exprToRtl (Call (Name name) args) nextReg scope = ((if nextReg > 0 then [Push reg_eax] else []) ++ 
                                               argsRtl ++
                                               handleArgPush argRegs ++
                                               [CallName ('_':name) argRegs nextReg] ++
                                               [AddConst reg_esp (toInteger $ length args * 4)] ++
                                               (if nextReg > 0 then [MovReg (nextReg + 1) reg_eax, Pop reg_eax] else []),
                                               nextReg + 1, typ)
    where
        (argsRtl, argRegs) = handleCallArgs args nextReg scope
        typ = Just $ funRetType $ fromJust $ scopeGetFun scope name

exprToRtl (Call addr args) nextReg scope = ((if nextReg > 0 then [Push reg_eax] else []) ++ 
                                        addrRtl ++
                                        argsRtl ++
                                        handleArgPush argRegs ++
                                        [CallAddr addrReg argRegs nextReg] ++
                                        [AddConst reg_esp (toInteger $ length args * 4)] ++
                                        (if nextReg > 0 then [MovReg (nextReg + 1) reg_eax, Pop reg_eax] else []),
                                        nextReg + 1, typ)
    where
        (addrRtl, addrReg, addrType) = exprToRtl addr nextReg scope
        (argsRtl, argRegs) = handleCallArgs args addrReg scope
        typ = case addrType of
            Just (PtrType (FuncType t _)) -> Just t
            _ -> error "Trying to call pointer to non function"

exprToRtl (ArrayDeref addr offset) nextReg scope =
    exprToRtl (App (getOpFromSym "$") [App (getOpFromSym "+") [addr, offset]]) nextReg scope

fixPtrOffset :: Reg -> Reg -> Type -> Rtl
fixPtrOffset reg nextReg typ =
    maybe [] (\ t -> [Mov (nextReg + 1) (getTypeSize t),
            Mul reg (nextReg + 1)])
        $ getPtrType typ

--simpleOp :: Ast -> Reg -> Scope -> (Rtl, Reg, Maybe Type)

getValueFromAst :: Ast -> Maybe Value
getValueFromAst (Number x) = Just $ Integer x
getValueFromAst (Literal x) = Just $ String x
getValueFromAst _ = error "Non-constant value in global definition"

argTypesToVars :: [(Type, String)] -> [Var]
argTypesToVars list = argTypesToVarsLoop list [] where
    argTypesToVarsLoop ((t,n):xs) res = argTypesToVarsLoop xs (res ++ [Var n t Nothing True])
    argTypesToVarsLoop [] res = res

handleArgPush :: [Reg] -> Rtl
handleArgPush regs = handleArgPushLoop regs [] where
    handleArgPushLoop (x:xs) rtl = handleArgPushLoop xs ((Push x):rtl)
    handleArgPushLoop [] rtl = rtl

handleCallArgs :: [Ast] -> Reg -> Scope -> (Rtl, [Reg])
handleCallArgs [] _ _ = ([], [])
handleCallArgs (x:xs) nextReg scope = (argRtl ++ finalRtl, argReg : finalReg)
    where
        (argRtl, argReg, _) = exprToRtl x nextReg scope
        (finalRtl, finalReg) = handleCallArgs xs argReg scope

handleAssign :: Ast -> Ast -> Reg -> Scope -> (Rtl, Reg)
handleAssign (Name name) expr nextReg scope =
    if scopeHasVar scope name
        then if canCast lType (fromJust rType)
            then let i = getOffset scope name in
                if isJust i
                    then (exprRtl ++ [SaveLoc (fromJust i) assignReg], assignReg)
                    else (exprRtl ++ [Save name assignReg 4], assignReg)
            else error $ "Cannot autocast " ++ (show $ fromJust rType) ++ " to " ++ (show lType)
        else error $ "Variable not in scope: " ++ name
    where
        (exprRtl, assignReg, rType) = exprToRtl expr nextReg scope
        lType = varType $ fromJust $ scopeGetVar scope name

handleAssign (App op [addrExpr]) expr nextReg scope =
    if canCast (fromJust $ getType "$" (fromJust lType) undefined) (fromJust rType)
        then (addrRtl ++ exprRtl ++ [SaveToPtr addrReg exprReg 4], exprReg)
        else error $ "Cannot autocast " ++ (show $ fromJust rType) ++ " to " ++ (show $ fromJust lType)
    where
        (addrRtl, addrReg, lType) = exprToRtl addrExpr nextReg scope
        (exprRtl, exprReg, rType) = exprToRtl expr addrReg scope

handleAssign (ArrayDeref addr offset) expr nextReg scope =
    handleAssign (App (getOpFromSym "$") [App (getOpFromSym "+") [addr, offset]]) expr nextReg scope

handleAddr :: Ast -> Reg -> Scope -> (Rtl, Reg)
handleAddr (Name name) nextReg scope =
    if isJust offset
        then ([AddrLoc (nextReg + 1) (fromJust offset)], nextReg + 1)
        else if scopeHasVar scope name
            then ([Addr (nextReg + 1) name], nextReg + 1)
            else if scopeHasFun scope name
                then ([Addr (nextReg + 1) ("_" ++ name)], nextReg + 1)
                else error $ "Undefined variable \"" ++ name ++ "\""
    where offset = getOffset scope name
handleAddr _ _ _ = error "Can only get address of lvalue"

getPtrType :: Type -> Maybe Type
getPtrType (PtrType t) = Just t
getPtrType (ArrayType t _) = Just t
getPtrType _ = Nothing

endsOnRet :: Ast -> Bool
endsOnRet (Block []) = False
endsOnRet (Block b) = case last b of
    (Return _) -> True
    _          -> False