module Compile ( compile ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative

import Scope
import Reg
import Type
import Pseudo
import Asm
import Ast
import qualified Id

compile :: File -> (Pseudo, Scope)
compile = runCompiler . compileFile

underscore = "_"

----- ENVIRONMENT -----

type Env = (Pseudo, Reg, Scope, Id.Id)

startReg = 0
emptyEnv = ([], startReg, emptyScope, ([], 0))

envGetAsm :: Env -> Pseudo
envGetAsm (s, _, _, _) = s

getRegFromEnv :: Env -> Reg
getRegFromEnv (_, r, _, _) = r

envFreeReg :: Env -> Env
envFreeReg (a, r, s, i) = (a, r - 1, s, i)

addLineToEnv :: PseudoLine -> Env -> Env
addLineToEnv l (xs, r, s, i) = (xs++[l], r, s, i)

nextReg :: Env -> Env
nextReg (env, r, s, i) = (env, r + 1, s, i)

envGetScope :: Env -> Scope
envGetScope (_, _, s, _) = s

envSetScope :: Scope -> Env -> Env
envSetScope s (a, r, _, i) = (a, r, s, i)

envGetId :: Env -> Id.Id
envGetId (_, _, _, i) = i

envSetId :: Id.Id -> Env -> Env
envSetId i (a, r, s, _) = (a, r, s, i)

----- BASIC MONAD -----

type Error = String
data Compiler a = C (Env -> Either (Env, a) Error)

instance Functor Compiler where
    fmap f (C a) = C $ \ inp -> case a inp of
        Left (inp', r) -> Left (inp', f r)
        Right e -> Right e
        
instance Applicative Compiler where
    pure x = C $ \ inp -> Left (inp, x)
    (<*>) (C a) (C b) = C $ \ inp -> case a inp of
        Left (inp', f) -> case b inp' of
            Left (inp'', r) -> Left (inp'', f r)
            Right e -> Right e
        Right e -> Right e

instance Alternative Compiler where
    empty = C $ \ _ -> Right ""
    (<|>) (C a) (C b) = C $ \ inp -> case a inp of
        Left r -> Left r
        Right e -> b inp
        
instance Monad Compiler where
    return x = C $ \ inp -> Left (inp, x)
    (>>=) (C a) b = C $ \ inp -> case a inp of
        Left (inp', r) -> let (C c) = b r in c inp'
        Right e -> Right e

failure :: Error -> Compiler a
failure e = C $ \ _ -> Right e

failIf :: Bool -> Error -> Compiler ()
failIf True e = failure e
failIf False _ = return ()

runCompiler :: Compiler () -> (Pseudo, Scope)
runCompiler (C c) = either (\ (e, _) -> (envGetAsm e, envGetScope e)) error $ c emptyEnv

loop :: (Monad m) => (a -> m ()) -> [a] -> m ()
loop _ [] = return ()
loop f (x:xs) = f x >> loop f xs

addLine :: PseudoLine -> Compiler ()
addLine l = C $ \ env -> Left (addLineToEnv l env, ())

addLines :: Pseudo -> Compiler ()
addLines = loop addLine

getReg :: Compiler Reg
getReg = C $ \ env -> Left (nextReg env, getRegFromEnv env)

freeReg :: Compiler ()
freeReg = C $ \ env -> Left (envFreeReg env, ())

borrowReg :: Compiler Reg
borrowReg = C $ \ env -> Left (env, getRegFromEnv env)

varExists :: String -> Compiler Bool
varExists str = getScope >>= return . flip scopeHasVar str

getVarType :: String -> Compiler Type
getVarType name = do
    sc <- getScope
    maybe (failure $ "Variable does not exist: " ++ name) (return . varType) $ scopeGetVar sc name

getFunType :: String -> Compiler Type
getFunType name = do
    (Fun _ r a _ _) <- getFun name
    return $ FuncType r a

getFun :: String -> Compiler Fun
getFun name = do
    sc <- getScope
    maybe (failure $ "Function does not exist: " ++ name) return $ scopeGetFun sc name

getVarOffset :: String -> Compiler Int
getVarOffset n = do
    sc <- getScope
    maybe (failure $ "Var not local: " ++ n) return $ getOffset sc n
    
addGlo :: Var -> Compiler ()
addGlo var = getScope >>= setScope . flip scopeAddGlo var

addStc :: Var -> Compiler ()
addStc var = getScope >>= setScope . flip scopeAddStc var

addPar :: Var -> Compiler ()
addPar var = getScope >>= setScope . flip scopeAddPar var

addLoc :: Var -> Compiler ()
addLoc var = getScope >>= setScope . flip scopeAddLoc var

addFun :: Fun -> Compiler ()
addFun fun = getScope >>= setScope . flip scopeAddFun fun

getScope :: Compiler Scope
getScope = C $ \ env -> Left (env, envGetScope env)

setScope :: Scope -> Compiler ()
setScope s = C $ \ env -> Left (envSetScope s env, ())

getId :: Compiler Id.Id
getId = C $ \ env -> Left (env, envGetId env)

setId :: Id.Id -> Compiler ()
setId i = C $ \ env -> Left (envSetId i env, ())

funcId :: String -> Compiler ()
funcId s = setId $ Id.funcId s

getFuncId :: Compiler String
getFuncId = getId >>= return . Id.getFuncId

getLoopId :: Compiler String
getLoopId = getId >>= (maybe (failure "Not in loop") return) . Id.getLoopId

addLoopId :: Compiler ()
addLoopId = getId >>= setId . Id.addLoopId

addIfId :: Compiler ()
addIfId = getId >>= setId . Id.addIfId

popId :: Compiler ()
popId = getId >>= setId . Id.popId

incId :: Compiler ()
incId = getId >>= setId . Id.incId

getIdString :: Compiler String
getIdString = getId >>= return . Id.getIdString

----- COMPLIE -----

compileFile :: File -> Compiler ()
compileSymb :: Symb -> Compiler ()
compileStmt :: Stmt -> Compiler ()
compileExpr :: Expr -> Compiler (Reg, Type)

compileFile (File s) = loop compileSymb s

compileSymb (VarDecl typ name False) = addGlo $ Var name typ Nothing True
compileSymb (VarDecl typ name True)  = addStc $ Var name typ Nothing True
compileSymb (Init typ name expr) = addGlo $ Var name typ (Just $ evaluate expr) True
compileSymb (FunDecl (FuncType retType args) name) = addFun $ Fun name retType args False 0

compileSymb (Func (FuncType retType args) name body) = do
    let numLocals = countLocals body
    addFun (Fun name retType args True numLocals)
    addLines [Label $ underscore ++ name, Push reg_ebp, MovReg reg_ebp reg_esp]
    when (numLocals > 0) $ addLine $ SubConst reg_esp $ toInteger numLocals
    funcId name
    sc <- getScope
    loop addPar $ map (\ (t, n) -> Var n t Nothing True) $ reverse args
    loop (\ s -> compileStmt s >> incId) body
    setScope sc
    when (not $ endsOnRet body) $ addLine $ Ret name
    popId

compileStmt (Block body) = do
    sc <- getScope
    loop (\ s -> compileStmt s >> incId) body
    setScope sc

compileStmt (LocVar typ name _ e) = do
    addLoc $ Var name typ Nothing True
    case e of
        Just ex -> compileExpr (App "=" [Name name, ex]) >> freeReg >> return ()
        Nothing -> return ()

compileStmt (If cond st1 st2) = do
    (reg, _) <- compileExpr cond
    id <- addIfId >> getIdString
    addLine $ Cmp reg
    freeReg
    addLine $ Je $ id ++ ".else"
    compileStmt st1
    addLine $ Jmp $ id ++ ".end"
    addLine $ Label $ id ++ ".else"
    compileStmt st2
    addLine $ Label $ id ++ ".end"
    popId

compileStmt (While cond body) = do
    id <- addLoopId >> getIdString
    addLine $ Label $ id ++ ".start"
    (reg, _) <- compileExpr cond
    addLine $ Cmp reg
    freeReg
    addLine $ Je $ id ++ ".end"
    compileStmt body
    addLine $ Jmp $ id ++ ".start"
    addLine $ Label $ id ++ ".end"
    popId

compileStmt (For pre cond post body) = do
    id <- addLoopId >> getIdString
    compileStmt pre
    addLine $ Jmp $ id ++ ".cond"
    addLine $ Label $ id ++ ".start"
    compileExpr post
    addLine $ Label $ id ++ ".cond"
    (reg, _) <- compileExpr cond
    addLine $ Cmp reg
    freeReg
    addLine $ Je $ id ++ ".end"
    compileStmt body
    addLine $ Jmp $ id ++ ".start"
    addLine $ Label $ id ++ ".end"
    popId
    
compileStmt Nop = return ()
compileStmt Break = getLoopId >>= addLine . Jmp . (++".end")
compileStmt Continue = getLoopId >>= addLine . Jmp . (++".start")
compileStmt (Goto str) = addLine $ Jmp str
compileStmt (GotoLabel str) = addLine $ Label str
compileStmt (Return Nothing) = getFuncId >>= addLine . Ret

compileStmt (Return (Just e)) = do
    (reg, typ) <- compileExpr e
    id <- getFuncId
    retType <- funRetType <$> getFun id
    tryCast retType typ
    when (reg /= reg_eax) $ addLine $ MovReg reg_eax reg
    addLine $ Ret id
    freeReg

compileStmt (Expr expr) = compileExpr expr >> freeReg >> return ()

compileExpr (Literal s) = do
    reg <- getReg
    addLine $ LoadLit reg s
    return (reg, PtrType $ PrimType "char")

compileExpr (Number x) = do
    reg <- getReg
    addLine $ Mov reg x
    return (reg, fromJust $ getIntType x)

compileExpr (Name name) = do
    r <- getReg
    t <- getVarType name <|> getFunType name
    loadArray t <|> loadLoc r <|> loadFun r <|> loadGlo r
    return (r, t)
    where
    loadArray t = do
        failIf (not $ typeIsArray t) ""
        void $ freeReg >> compileExpr (App "&" [Name name])
    loadLoc r = do 
        i <- getVarOffset name
        addLine $ LoadLoc r $ toInteger i
    loadGlo r = getVarType name >> (addLine $ Load r name)
    loadFun r = getFunType name >> (addLine $ Load r $ underscore ++ name)

compileExpr (App "+" [expr1, expr2]) = handleSummation "+" expr1 expr2
compileExpr (App "-" [expr1, expr2]) = handleSummation "-" expr1 expr2

compileExpr (App "&" [Name name]) = do
    reg <- getReg
    typ <- ((AddrLoc reg) <$> toInteger <$> (getVarOffset name) >>= addLine >> getVarType name)
        <|> (getVarType name <* (addLine $ Addr reg name))
        <|> (getFunType name <* (addLine $ Addr reg $ underscore ++ name))
        <|> failure "Can only get address of l-value"
    return (reg, PtrType typ)

compileExpr (App "=" [Name name, expr]) = do
    (reg, typ) <- compileExpr expr
    varTyp     <- getVarType name
    tryCast varTyp typ
    (SaveLoc reg <$> toInteger <$> getVarOffset name >>= addLine)
        <|> (addLine $ Save name reg $ toInteger $ getTypeSize varTyp)
    return (reg, varTyp)

compileExpr (App "=" [App "$" [addrExpr], valueExpr]) = do
    (addrReg, ptrTyp)    <- compileExpr addrExpr
    (valueReg, valueTyp) <- compileExpr valueExpr
    addrTyp              <- deref ptrTyp
    tryCast addrTyp valueTyp
    addLine $ SaveToPtr addrReg valueReg $ toInteger $ getTypeSize addrTyp
    freeReg
    return (addrReg, addrTyp)

compileExpr (App "=" [ArrayDeref addrExpr iEx, expr]) = do
    (addrReg, ptrTyp)    <- compileExpr addrExpr
    (valueReg, valueTyp) <- compileExpr expr
    (iReg, iTyp)         <- compileExpr iEx
    addrTyp              <- deref ptrTyp
    tryCast addrTyp valueTyp
    fixPtrOffset iReg addrTyp
    failIf (not $ typeIsInt iTyp) "Array index must be int"
    addLine $ Add addrReg iReg
    addLine $ SaveToPtr addrReg valueReg $ toInteger $ getTypeSize addrTyp
    freeReg
    freeReg
    return (addrReg, addrTyp)

compileExpr (App "++" [expr]) = compileExpr $ App "=" [expr, App "+" [expr, Number 1]]
compileExpr (App "--" [expr]) = compileExpr $ App "=" [expr, App "-" [expr, Number 1]]

compileExpr (App "+++" [expr]) = do
    (reg, typ) <- compileExpr $ App "=" [expr, App "+" [expr, Number 1]]
    addLine $ Dec reg
    return (reg, typ)

compileExpr (App "---" [expr]) = do
    (reg, typ) <- compileExpr $ App "=" [expr, App "-" [expr, Number 1]]
    addLine $ Inc reg
    return (reg, typ)

compileExpr (App sym [expr]) = do
    (reg, typ) <- compileExpr expr
    let retType = getType sym typ undefined
    failIf (retType == Nothing) "Incompatible type"
    case sym of
        "$" -> addLine $ DeRef reg
        "!" -> addLines [Test reg, Setz reg, AddConst reg 1]
    return (reg, fromJust retType)

compileExpr (App sym [expr1, expr2]) = do
    (reg1, type1) <- compileExpr expr1
    (reg2, type2) <- compileExpr expr2
    let retType = getType sym type1 type2
    failIf (retType == Nothing) "Incompatible types"
    case sym of
        "*" -> addLine $ Mul reg1 reg2
        "/" -> addLine $ Div reg1 reg2
        "==" -> addLines [Sub reg1 reg2, Setz reg1, AndConst reg1 1]
        "!=" -> addLine $ Sub reg1 reg2
        ">" -> addLines [Sub reg1 reg2, Setg reg1, AndConst reg1 1]
        "<" -> addLines [Sub reg1 reg2, Setl reg1, AndConst reg1 1]
        ">=" -> addLines [Sub reg1 reg2, Setge reg1, AndConst reg1 1]
        "<=" -> addLines [Sub reg1 reg2, Setle reg1, AndConst reg1 1]
        "|" -> addLine $ Or reg1 reg2
        "^" -> addLine $ Xor reg1 reg2
        "&" -> addLine $ And reg1 reg2
        "<<" -> addLines [Push reg_ecx, MovReg reg_ecx reg2, Shl reg1, Pop reg_ecx]
        ">>" -> addLines [Push reg_ecx, MovReg reg_ecx reg2, Shr reg1, Pop reg_ecx]
    freeReg
    return (reg1, fromJust retType)

compileExpr (ArrayDeref ex i) = do
    (reg, typ) <- compileExpr ex
    newTyp <- deref typ
    (iReg, iTyp) <- compileExpr i
    failIf (not $ typeIsInt iTyp) $ "Array index must be integer"
    fixPtrOffset iReg typ
    addLines [Add reg iReg, DeRef reg]
    freeReg
    return (reg, newTyp)

compileExpr (Ternary cond expr1 expr2) = do
    (reg, _) <- compileExpr cond
    id <- addIfId >> getIdString
    addLine $ Cmp reg
    freeReg
    addLine $ Je $ id ++ ".else"
    (reg2, typ2) <- compileExpr expr1
    addLine $ Jmp $ id ++ ".end"
    addLine $ Label $ id ++ ".else"
    (reg3, typ3) <- compileExpr expr2
    addLine $ MovReg reg2 reg3
    addLine $ Label $ id ++ ".end"
    freeReg
    popId
    return (reg2, typ2)

compileExpr c@(Call ex args) = callByName c <|> callByAddr c

callByName (Call (Name name) args) = do
    retType <- funRetType <$> getFun name
    reg <- getReg
    when (reg /= reg_eax) $ addLine $ Push reg_eax
    handleCallArgs args
    addLine $ CallName (underscore ++ name) [] 0
    when (length args > 0) $ addLine $ AddConst reg_esp $ toInteger $ length args * 4
    when (reg /= reg_eax) $ addLines [MovReg reg reg_eax, Pop reg_eax]
    return (reg, retType)

callByAddr (Call ex args) = do
    (reg, typ) <- compileExpr ex
    case typ of
        (PtrType (FuncType _ _)) -> return ()
        _ -> failure $ (show typ) ++ "Not a pointer to a function"
    when (reg /= reg_eax) $ addLine $ Push reg_eax
    handleCallArgs args
    addLine $ CallAddr reg [] 0
    when (length args > 0) $ addLine $ AddConst reg_esp $ toInteger $ length args * 4
    when (reg /= reg_eax) $ addLines [MovReg reg reg_eax, Pop reg_eax]
    return (reg, typ)

handleSummation :: String -> Expr -> Expr -> Compiler (Reg, Type)
handleSummation s expr1 expr2 = do
    (reg1, type1) <- compileExpr expr1
    (reg2, type2) <- compileExpr expr2
    let retType = getType s type1 type2
    failIf (not $ isJust retType) "Incompatible types"
    fixPtrOffset reg1 type2
    fixPtrOffset reg2 type1
    addLine $ (if s == "+" then Add else Sub) reg1 reg2
    freeReg
    return (reg1, fromJust retType)

handleCallArgs :: [Expr] -> Compiler ()
handleCallArgs = loop (\ x -> compileExpr x >>= addLine <$> Push <$> fst >> freeReg) . reverse
    
fixPtrOffset :: Reg -> Type -> Compiler ()
fixPtrOffset reg1 typ =
    maybe (return ())
        (\ t -> do
            reg2 <- borrowReg
            addLine $ Mov reg2 $ toInteger $ getTypeSize t
            addLine $ Mul reg1 reg2
        )
        $ getPtrType typ

tryCast :: Type -> Type -> Compiler ()
tryCast l r = failIf (not $ canCast l r)  $ "Cannot autocast from " ++ show l ++ " to " ++ show r

deref :: Type -> Compiler Type
deref = maybe (failure "Cannot deref non-pointer") return . getPtrType

countLocals :: [Stmt] -> Int
countLocals [] = 0
countLocals ((LocVar typ _ False _):xs) = getTypeSize typ + countLocals xs
countLocals ((Block e):xs) = countLocals e + countLocals xs
countLocals ((If _ e1 e2):xs) = countLocals [e1] + countLocals [e2] + countLocals xs
countLocals (_:xs) = countLocals xs

evaluate :: Expr -> Value
evaluate (Number x) = Integer $ fromInteger x
evaluate (Literal s) = String s

endsOnRet :: [Stmt] -> Bool
endsOnRet [] = False
endsOnRet ([Return _]) = True
endsOnRet ([Block b]) = endsOnRet b
endsOnRet (x:xs) = endsOnRet xs