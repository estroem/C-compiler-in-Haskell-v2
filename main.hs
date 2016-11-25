{-
 - TODO
 - 
 - Register handling
 - Arrays
 - Void
 - Break / Continue
 - Floats
 - 
 -}

import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data Ast = Number Integer | Name String | App Op [Ast] | Block [Ast] | VarDecl Type String Bool
         | If Ast Ast Ast | Call Ast [Ast] | Init Type String Ast
         | Func Type String Ast | File [Ast] | FunDecl Type String | Literal String | Return (Maybe Ast)
         | While Ast Ast | ArrayDeref Ast Ast
    deriving (Show)

data Op = Op
    { symbol :: String
    , numArgs :: Integer
    , precedence :: Integer
    , assoc :: Integer
    }

data ExprElem = Operator Op | Ast Ast
    deriving (Show)

instance Show Op where
    show (Op {symbol=s}) = show s

data Type = PrimType String | PtrType Type | FuncType Type [(Type, String)] | ArrayType Type | EmptyType

instance Eq Type where
    (PtrType a) == (PtrType b) = a == b
    (PrimType a) == (PrimType b) = a == b
    (FuncType a b) == (FuncType c d) = a == c && (fst $ unzip b) == (fst $ unzip d)
    (ArrayType a) == (ArrayType b) = a == b
    EmptyType == EmptyType = True
    _ == _ = False

instance Show Type where
    show t = showType t ""

data IdElem = FuncId String | LoopId Integer | IfId Integer

instance Show IdElem where
    show (FuncId s) = s
    show (LoopId i) = "loop" ++ show i
    show (IfId i) = "if" ++ show i

type Id = ([IdElem], Integer)

data RtlLine = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer
             | Load Reg String | Save String Reg Integer | SaveToPtr Reg Reg Integer | Label String
             | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
             | CallName String [Reg] Reg | CallAddr Reg [Reg] Reg | DeRef Reg
             | FuncStart String | FuncEnd String | Ret String | Push Reg | LoadLoc Reg Integer
             | SaveLoc Integer Reg | Pop Reg | AddConst Reg Integer | SubConst Reg Integer
             | MulConst Reg Integer | DivConst Reg Integer | LoadLit Reg Lit | MovReg Reg Reg
             | Addr Reg String | AddrLoc Reg Integer
    deriving (Show)
type Reg = Integer

type Rtl = [RtlLine]

type Register = Integer
type Registers = [Bool]

data Var = Var
    { varName :: String
    , varType :: Type
    , varValue :: Maybe Value
    , varIsVis :: Bool
    } deriving (Show)

data Fun = Fun
    { funName :: String
    , funRetType :: Type
    , funArgs :: [(Type, String)]
    , funIsDef :: Bool
    , numLocals :: Integer
    } deriving (Show)

data Value = Integer Integer | Float Float | String String
    deriving (Show)

type Lit = String

reg_ebp :: Integer
reg_ebp = -2
reg_esp :: Integer
reg_esp = -1
reg_eax :: Integer
reg_eax = -3

--           Scope globals statics locals functions
data Scope = Scope [Var] [Var] [Var] [Var] [Fun]
    deriving (Show)

type AsmLine = String
type Asm = [AsmLine]

operators = [(Op "+" 2 1 0), (Op "-" 2 1 0), (Op "*" 1 2 0), (Op "/" 2 2 0), (Op "++" 1 3 0), (Op "=" 2 0 0), (Op "$" 1 4 0),{- (Op "==" 2 0 0),-} (Op "!=" 2 0 0), (Op "&" 1 4 0)]
extraSymbols = [";", "(", ")", "{", "}", ",", "[", "]"]

opShoRtlist = ["+", "-", "*", "/", "++", "=", "$", "==", "!=", "&"]

prims = [("int", 4), ("short", 2), ("char", 1), ("float", 4), ("double", 8)]
typeShoRtlist = ["int", "short", "byte", "char"]

--- TOKENIZE

tokenize :: String -> [String]
tokenize [] = []
tokenize ('/':'/':xs) = tokenize $ dropWhile (/= '\n') xs
tokenize (x:xs)
    | isDigit x = (x : takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = (x : takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
    | x == '"' = (x : takeWhile (/= '"') xs) : tokenize (tail $ dropWhile (/= '"') xs)
    | x == ' ' || x == '\t' || x == '\n' = tokenize xs
    | length xs >= 2 && symExists (x:take 2 xs) = (x : (take 2 xs)) : (tokenize (drop 2 xs))
    | length xs >= 1 && symExists (x:take 1 xs) = (x : (take 1 xs)) : (tokenize (tail xs))
    | symExists [x] = [x] : tokenize xs
    | otherwise = error $ "Illegal symbol \"" ++ [x] ++ "\""

symExists :: String -> Bool
symExists sym = elem sym (opShoRtlist ++ extraSymbols)

--- PARSE

parse :: String -> Ast
parse str = fst $ parseFile $ tokenize str

addAst :: Ast -> Ast -> Ast
addAst (Block list) ast = Block (ast:list)
addAst (File list) ast = File (ast:list)

parseFile :: [String] -> (Ast, [String])
parseFile [] = (File [], [])
parseFile (x:xs) = (addAst file line, final)
    where
        (line, rest) = parseTopLvlLine (x:xs)
        (file, final) = parseFile rest

parseTopLvlLine :: [String] -> (Ast, [String])
parseTopLvlLine (x:xs) =
    case decl of
        (FuncType _ _) -> if (head rest) == ";"
                              then (FunDecl decl name, tail rest)
                              else let block = parseLineOrBlock rest
                                  in (Func decl name (fst block), snd block)
        _              -> if (head rest) == "="
                            then (Init decl name expr, tail exprRest)
                            else if (head rest) == ";"
                                then (VarDecl decl name False, tail rest)
                                else error "Expected ; or ="
    where
        (decl, name, rest) = parseDecl (x:xs)
        (expr, exprRest) = parseExpr $ drop 1 rest

parseDecl :: [String] -> (Type, String, [String])
parseDecl (x:xs) = (addType a (PrimType x), b, c)
    where (a, b, c) = parseDeclReq xs

parseDeclReq :: [String] -> (Type, String, [String])
parseDeclReq ("(":xs) = 
    if (head afterDef) == ")"
        then if (afterDef !! 1) == "("
            then (addType def (FuncType EmptyType args), name, afterArgs)
            else (def, name, tail afterDef)
        else error $ "Unexpected " ++ (head afterDef) ++ ", exptected )"
    where
        (def, name, afterDef) = parseDeclReq xs
        (args, afterArgs) = parseFuncArgs $ drop 2 afterDef

parseDeclReq (")":xs) = (EmptyType, "", ")":xs)
parseDeclReq ("*":xs) = (addType a (PtrType EmptyType), b, c)
    where
        (a, b, c) = parseDeclReq xs

parseDeclReq (x:xs) =
    if (head xs) == "("
        then ((FuncType EmptyType args), x, rest)
        else if isAlpha $ head x then (EmptyType, x, xs) else error $ "Illegal variable \"" ++ x ++ "\". Variables must start with letter"
    where
        (args, rest) = parseFuncArgs $ tail xs

addType :: Type -> Type -> Type
addType (PrimType a) b = error ""
addType EmptyType b = b
addType (PtrType a) b = (PtrType (addType a b))
addType (FuncType a c) b = (FuncType (addType a b) c)

parseFuncArgs :: [String] -> ([(Type, String)], [String])
parseFuncArgs (")":xs) = ([], xs)
parseFuncArgs (",":xs) = parseFuncArgs xs
parseFuncArgs (x:xs) = ((a, b) : argList, rest)
    where
        (a, b, c) = parseDecl (x:xs)
        (argList, rest) = parseFuncArgs c
    
parseBlock :: [String] -> (Ast, [String])
parseBlock [] = (Block [], [])
parseBlock (";":xs) = (Block [], xs)
parseBlock ("}":xs) = (Block [], xs)
parseBlock (x:xs) = (addAst (fst block) (fst expr), snd block)
    where
        expr = parseLine (x:xs)
        block = parseBlock $ snd expr

parseLine :: [String] -> (Ast, [String])
parseLine [] = (undefined, [])
parseLine ("if":"(":xs) = parseIf xs
parseLine ("while":"(":xs) = parseWhile xs
parseLine ("return":r)
    | (head r) == ";" = (Return Nothing, tail r)
    | otherwise = (Return $ Just expr, drop 1 r2)
    where
        (expr, r2) = parseExpr r
parseLine (x:xs)
    | isType x = (VarDecl decl name False, if (head rest) == ";" then tail rest else name:rest)
    | otherwise = (fst expr, drop 1 $ snd expr)
        where
            (decl, name, rest) = parseDecl (x:xs)
            expr = parseExpr (x:xs)

isType :: String -> Bool
isType str = elem str typeShoRtlist

parseIf :: [String] -> (Ast, [String])
parseIf (x:xs) =
    if length (snd block1) > 0 && (snd block1) !! 0 == "else"
        then (If (fst expr) (fst block1) (fst block2), snd block2)
        else (If (fst expr) (fst block1) (Block []), snd block1)
    where
        expr = parseExpr (x:xs)
        block1 = parseLineOrBlock $ drop 1 $ snd expr
        block2 = parseLineOrBlock $ drop 1 $ snd block1

parseWhile :: [String] -> (Ast, [String])
parseWhile (x:xs) = (While (fst cond) (fst block), snd block)
    where
        cond = parseExpr (x:xs)
        block = parseLineOrBlock $ drop 1 $ snd cond

parseCallArgs :: [String] -> ([Ast], [String])
parseCallArgs ("(":")":xs) = ([], xs)
parseCallArgs (")":xs) = ([], xs)
parseCallArgs (x:xs) = (arg : nextArgs, rest)
    where
        (arg, nextArgsString) = parseExpr xs
        (nextArgs, rest) = parseCallArgs nextArgsString

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = (undefined, [])
parseExpr (x:xs) = (prefixToTree $ infixToPrefix $ fst exprList, snd exprList)
    where
        exprList = getExprList (x:xs)

parseLineOrBlock :: [String] -> (Ast, [String])
parseLineOrBlock (";":xs) = (Block [], xs)
parseLineOrBlock ("{":xs) = parseBlock xs
parseLineOrBlock (x:xs) = (Block [fst line], snd line)
    where line = parseLine (x:xs)

parseSingleExpr :: [String] -> (Ast, [String])
parseSingleExpr (x:xs) =
    let expr = if x == "("
        then parseExpr xs
        else if all isDigit x
            then (Number $ read x, (x:xs))
            else if all isAlpha x
                then (Name x, (x:xs))
                else if (head x) == '"'
                    then (Literal $ tail x, (x:xs))
                    else error $ "Unexpected \"" ++ x ++ "\""
        in
            if (snd expr) !! 1 == "("
                then
                    let args = (parseCallArgs $ tail $ snd expr)
                        in (Call (fst expr) (fst args), snd args)
                else if (snd expr) !! 1 == "["
                    then
                        let offset = parseExpr $ drop 2 (snd expr)
                            in (ArrayDeref (fst expr) (fst offset), drop 1 $ snd offset)
                    else (fst expr, tail $ snd expr)

getExprList :: [String] -> ([ExprElem], [String])
getExprList [] = ([], []) -- error "Unexpected end of expression"
getExprList (x:xs)
    | x == ")" || x == "," || x == ";" || x == "]" = ([], (x:xs))
    | elem x opShoRtlist = let exprList = getExprList xs in ((Operator (getOpFromSym x)) : fst exprList, snd exprList)
    | otherwise =  let exprList = getExprList (snd expr) in ((Ast (fst expr)) : fst exprList, snd exprList)
        where
            expr = parseSingleExpr (x:xs)

prefixToTree :: [ExprElem] -> Ast
prefixToTree list = fst $ prefixToTreeReq list

prefixToTreeReq :: [ExprElem] -> (Ast, [ExprElem])
prefixToTreeReq ((Ast x):xs) = (x, xs)
prefixToTreeReq ((Operator op):xs) =
    if numArgs op == 2
        then (App op ((fst arg2):(fst arg1):[]), snd arg2)
        else if numArgs op == 1
            then (App op [fst arg1], snd arg1)
            else error "Illegal number of args"
    where
        arg1 = prefixToTreeReq xs
        arg2 = prefixToTreeReq $ snd arg1

infixToPrefix :: [ExprElem] -> [ExprElem]
infixToPrefix list = reverse opList ++ valueList
    where
        (opList, valueList) = infixToPostfixReq list [] []

infixToPostfixReq :: [ExprElem] -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
infixToPostfixReq [] a b = (a, b)
infixToPostfixReq ((Ast x):xs) opList valueList = infixToPostfixReq xs opList ((Ast x):valueList)
infixToPostfixReq ((op@(Operator {})):xs) opList valueList = infixToPostfixReq xs (op:opList2) valueList2
    where
        (opList2, valueList2) = popOperators op opList valueList

popOperators :: ExprElem -> [ExprElem] -> [ExprElem] -> ([ExprElem], [ExprElem])
popOperators (Operator op) opList valueList =
    if not (null opList) && (precedence op) < (precedence $ getOpFromExprElem $ head opList)
        then popOperators (Operator op) (tail opList) ((head opList) : valueList)
        else (opList, valueList)

getOpFromExprElem :: ExprElem -> Op
getOpFromExprElem (Operator op) = op

getOpFromSym :: String -> Op
getOpFromSym sym = fromJust $ find (\ op -> symbol op == sym) operators

getTypeFromSym :: String -> Type
getTypeFromSym sym = (PrimType sym) --fromJust $ find (\ n -> name n == sym) types

--- TO RTL

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

getOffset :: Scope -> String -> Maybe Integer
getOffset (Scope _ _ ps ls _) n =
    let i = findIndex (\ v -> (varName v) == n) ls in
        if isJust i
            then Just (toInteger $ (fromJust i + 1) * (-4))
            else let j = findIndex (\ v -> (varName v) == n) ps in
                if isJust j
                    then Just (toInteger $ (fromJust j + 2) * 4)
                    else Nothing

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
                                                                    (if numLocals > 0
                                                                        then [SubConst reg_esp (numLocals * 4)]
                                                                        else []) ++
                                                                    bodyRtl ++
                                                                    (if endsOnRet body
                                                                        then []
                                                                        else [Ret name]),
                                                                    (Scope [] ss [] [] [thisFun]))
    where
        (bodyRtl, (Scope _ ss _ ls _)) = blockToRtl body (joinScopes [(Scope [] [] (argTypesToVars argTypes) [] [thisFun]), scope]) (funcId name)
        numLocals = toInteger $ length ls
        thisFun = Fun name retType argTypes True numLocals


blockToRtl :: Ast -> Scope -> Id -> (Rtl, Scope)
blockToRtl (Block []) scope _ = ([], emptyScope)
blockToRtl (Block [x]) scope id = lineToRtl x scope id
blockToRtl (Block (x:xs)) scope id = (expr ++ block, hideLocals $ joinScopes [scope1, scope2])
    where
        (expr, scope1) = lineToRtl x scope id
        (block, scope2) = blockToRtl (Block xs) (joinScopes [scope, scope1]) (incId id)


lineToRtl :: Ast -> Scope -> Id -> (Rtl, Scope)

lineToRtl (If cond thenBlock elseBlock) scope id
    | not $ isEmpty elseBlock = (condRtl ++
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

lineToRtl (Return Nothing) _ id = ([Ret $ getFuncId id], emptyScope)

lineToRtl (Return (Just expr)) scope id =
    if canCast retType (fromJust exprType)
        then (exprRtl ++ [MovReg reg_eax reg, Ret $ getFuncId id], emptyScope)
        else error $ "Cannot autocast " ++ (show $ fromJust exprType) ++ " to " ++ (show retType)
    where
        (exprRtl, reg, exprType) = exprToRtl expr 0 scope
        retType = funRetType $ fromJust $ scopeGetFun scope $ getFuncId id

lineToRtl (VarDecl t n s) _ _ = ([], (if s then scopeAddStc else scopeAddLoc) emptyScope (Var n t Nothing True))

lineToRtl a b _ = let (c, _, _) = exprToRtl a 0 b in (c, emptyScope)


exprToRtl :: Ast -> Reg -> Scope -> (Rtl, Reg, Maybe Type)

exprToRtl (Number x) nextReg scope = ([Mov (nextReg + 1) x], nextReg + 1, getIntType x)

exprToRtl (Literal l) nextReg _ = ([LoadLit (nextReg + 1) l], nextReg + 1, Just $ PtrType $ PrimType "char")

exprToRtl (Name name) nextReg scope =
    if scopeHasVar scope name
        then let i = getOffset scope name in
            if isJust i
                then ([LoadLoc (nextReg + 1) (fromJust i)], nextReg + 1, varTyp)
                else ([Load (nextReg + 1) name], nextReg + 1, varTyp)
        else if scopeHasFun scope name
            then ([Load (nextReg + 1) name], nextReg + 1, funTyp)
            else error $ "Variable not in scope: " ++ name
    where
        varTyp = Just $ varType $ fromJust $ scopeGetVar scope name
        funTyp = Just $ FuncType (funRetType fun) (funArgs fun)
        fun = fromJust $ scopeGetFun scope name

exprToRtl (App op exprList) nextReg scope =
    (fst a, snd a, if (numArgs op) == 1
                       then getType (symbol op) (fromJust typ) undefined
                       else getType (symbol op) (fromJust lType) (fromJust rType))
    where
        (expr1, reg1, rType) = exprToRtl (exprList !! 1) nextReg scope
        (expr2, reg2, lType) = exprToRtl (exprList !! 0) reg1 scope
        (expr, reg, typ) = exprToRtl (head exprList) nextReg scope
        a = case symbol op of
            "+" -> (expr1 ++ expr2 ++ [Add reg2 reg1], reg2)
            "-" -> (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2)
            "*" -> (expr1 ++ expr2 ++ [Mul reg2 reg1], reg2)
            "/" -> (expr1 ++ expr2 ++ [Div reg2 reg1], reg2)
            "=" -> handleAssign (head exprList) (last exprList) nextReg scope
            "$" -> (expr ++ [DeRef reg], reg)
        --  "==" -> (expr1 ++ expr2 ++ [Cmp reg2 reg1], reg2)
            "!=" -> (expr1 ++ expr2 ++ [Sub reg2 reg1], reg2)
            "&" -> handleAddr (head exprList) nextReg scope

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
    if canCast (fromJust lType) (fromJust rType)
        then (addrRtl ++ exprRtl ++ [SaveToPtr addrReg exprReg 4], exprReg)
        else error $ "Cannot autocast " ++ (show $ fromJust rType) ++ " to " ++ (show $ fromJust lType)
    where
        (addrRtl, addrReg, lType) = exprToRtl addrExpr nextReg scope
        (exprRtl, exprReg, rType) = exprToRtl expr addrReg scope

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

isEmpty :: Ast -> Bool
isEmpty (Block list) = null list

endsOnRet :: Ast -> Bool
endsOnRet (Block []) = False
endsOnRet (Block b) = case last b of
    (Return _) -> True
    _          -> False

-- ID

funcId :: String -> Id
funcId n = ([FuncId n], 0)

getFuncId :: Id -> String
getFuncId id = case (fst id) !! 0 of
    (FuncId n) -> n
    _ -> error "Id doesn't start with func"

getLoopId :: Id -> Maybe String
getLoopId id = if null id' then Nothing else Just $ getIdString (id', 0)
    where id' = reverse $ dropWhile (\ e -> case e of { LoopId i -> False; _ -> True }) $ reverse $ fst id

addLoopId :: Id -> Id
addLoopId id = ((fst id) ++ [LoopId $ snd id], 0)

addIfId :: Id -> Id
addIfId id = ((fst id) ++ [IfId $ snd id], 0)

incId :: Id -> Id
incId id = (fst id, (snd id) + 1)

getIdString :: Id -> String
getIdString id = intercalate "." $ map show $ fst id

-- TYPECHECK

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
        _ -> error "Can not dereference non-pointer type"
    "&" -> Just $ PtrType l
    "!=" -> Just $ PrimType "int"

getMulType :: Type -> Type -> Maybe Type
getMulType (PtrType _) _ = Nothing
getMulType _ (PtrType _) = Nothing
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
showType (ArrayType t) str = showType t $ str ++ "[]"
showType EmptyType _ = "EmptyType"

-- TO ASM

toAsm :: Rtl -> Scope -> Asm
toAsm r scope@(Scope gs ss _ _ fs) = toAsmExtern fs ++ toAsmGlobals fs ++
                               ["section .data"] ++ (map toAsmDataLine $ gs ++ ss) ++
                               ["section .rodata", "?strings:"] ++ (map toAsmLitLine lits) ++
                               ["section .text"] ++ asm
    where
        (asm, lits) = toAsmLines $ map (retNumLocals scope) r

toAsmGlobals :: [Fun] -> Asm
toAsmGlobals funs = map (\ f -> "global _" ++ funName f) (filter funIsDef funs)

toAsmExtern :: [Fun] -> Asm
toAsmExtern funs = map (\ f -> "extern _" ++ funName f) (filter (\f -> (not $ funIsDef f) && (not $ any (\f2 -> funName f == funName f2 && funIsDef f2) funs)) funs)

toAsmDataLine :: Var -> AsmLine
toAsmDataLine (Var n t v _) = n ++ ": " ++ (getSizeWordData $ getSizeInt t) ++ " " ++
    case v of
        Nothing -> "0"
        Just (Integer x) -> show x
        Just (String x) -> "$ + 4" ++ "\ndb '" ++ x ++ "', 0"

toAsmLitLine :: Lit -> String
toAsmLitLine l = "db `" ++ l ++ "`, 0"

toAsmLines :: Rtl -> (Asm, [Lit])
toAsmLines rtl = toAsmLinesLoop rtl 0 [] [] where
    toAsmLinesLoop [] _ asm lits = (asm, lits)
    toAsmLinesLoop (x:xs) i asm lits = toAsmLinesLoop xs (i+1) (asm++lines) (lits++newLit)
        where
            (lines, newLit) = toAsmLine x i lits

toAsmLine :: RtlLine -> Integer -> [Lit] -> ([AsmLine], [Lit])
toAsmLine (Add reg1 reg2) _ _        = (["add " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Sub reg1 reg2) _ _        = (["sub " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Mul reg1 reg2) _ _        = (["mul " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Div reg1 reg2) _ _        = (["div " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Mov reg i) _ _            = (["mov " ++ getReg reg  ++ ", " ++ show i], [])
toAsmLine (Load reg name) _ _        = (["mov " ++ getReg reg  ++ ", [" ++ name ++ "]"], [])
toAsmLine (Save name reg size) _ _   = (["mov " ++ (getSizeWord size) ++ " [" ++ name ++ "], " ++ getReg reg], [])
toAsmLine (SaveToPtr reg1 reg2 size) _ _ = (["mov " ++ (getSizeWord size) ++ " [" ++ getReg reg1 ++ "], " ++ getReg reg2], [])
toAsmLine (Label name) _ _           = ([name ++ ":"], [])
toAsmLine (Cmp reg) _ _              = (["cmp " ++ getReg reg ++ ", 0"], [])
toAsmLine (Jmp label) _ _            = (["jmp " ++ label], [])
toAsmLine (Je label) _ _             = (["je " ++ label], [])
toAsmLine (Jne label) _ _            = (["jne " ++ label], [])
toAsmLine (Jle label) _ _            = (["jle " ++ label], [])
toAsmLine (Jl label) _ _             = (["jl " ++ label], [])
toAsmLine (CallName name args _) _ _ = (["call " ++ name], [])
toAsmLine (CallAddr addr args _) _ _ = (["call " ++ getReg addr], [])
toAsmLine (DeRef reg) _ _            = (["mov " ++ getReg reg ++ ", [" ++ getReg reg ++ "]"], [])
toAsmLine (Ret i) _ _                = ((if (read i) > 0 then ["add " ++ getReg reg_esp ++ ", " ++ i] else []) ++ ["pop " ++ getReg reg_ebp, "ret"], [])
toAsmLine (Push reg) _ _             = (["push " ++ getReg reg], [])
toAsmLine (Pop reg) _ _              = (["pop " ++ getReg reg], [])
toAsmLine (LoadLoc reg offset) _ _   = (["mov " ++ getReg reg ++ ", [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"], [])
toAsmLine (SaveLoc offset reg) _ _   = (["mov [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "], " ++ getReg reg], [])
toAsmLine (AddConst reg int)  _ _    = (["add " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (SubConst reg int) _ _     = (["sub " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (MulConst reg int) _ _     = (["mul " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (DivConst reg int) _ _     = (["div " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (LoadLit reg l) _ ls       = (["mov " ++ getReg reg ++ ", ?strings + " ++ show (litsGetSize ls)], [l])
toAsmLine (MovReg reg1 reg2) _ _     = (["mov " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Addr reg name) _ _        = (["mov " ++ getReg reg ++ ", " ++ name], [])
toAsmLine (AddrLoc reg offset) _ _   = (["lea " ++ getReg reg ++ ", [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"], [])

retNumLocals :: Scope -> RtlLine -> RtlLine
retNumLocals s (Ret n) = Ret $ show $ (funcGetNumLoc s n) * 4
retNumLocals _ a = a

litsGetSize :: [Lit] -> Integer
litsGetSize list = foldr (\ l s -> s + toInteger (length l) + 1) 0 list

funcGetNumLoc :: Scope -> String -> Integer
funcGetNumLoc (Scope _ _ _ _ fs) n = numLocals $ fromJust $ find (\f -> (funName f) == n) fs

getReg :: Reg -> String
getReg (-3) = "eax"
getReg (-2) = "ebp"
getReg (-1) = "esp"
getReg 1 = "eax"
getReg 2 = "ebx"
getReg 3 = "ecx"
getReg 4 = "edx"

getSizeInt :: Type -> Integer
getSizeInt (PtrType _) = 4
getSizeInt (PrimType "int") = 4
getSizeInt (PrimType "short") = 2
getSizeInt (PrimType "byte") = 1
getSizeInt (PrimType "char") = 1

getSizeWordData :: Integer -> String
getSizeWordData 1 = "db"
getSizeWordData 2 = "dw"
getSizeWordData 4 = "dd"

getSizeWord :: Integer -> String
getSizeWord 1 = "byte"
getSizeWord 2 = "word"
getSizeWord 4 = "dword"

--- COMPILE

compile :: String -> Asm
compile = (uncurry toAsm) . toRtl . parse

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 2
        then do
            file <- readFile $ (args !! 0)
            writeFile (args !! 1) $ intercalate "\n" $ compile $ file
        else
            putStrLn $ intercalate "\n" $ compile $ (head args)