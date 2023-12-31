module Compile ( compile ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Debug.Trace

import Scope
import Reg
import Type
import Pseudo
import Asm
import Ast
import qualified Id
import Env

compile :: File -> (Pseudo, Scope, [Lit], [Float])
compile = runCompiler . compileFile

underscore = ""

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

runCompiler :: Compiler () -> (Pseudo, Scope, [Lit], [Float])
runCompiler (C c) = either (envGetResult . fst) error $ c emptyEnv

loop :: (Monad m) => (a -> m ()) -> [a] -> m ()
loop _ [] = return ()
loop f (x:xs) = f x >> loop f xs

ifElseM :: (Monad m) => m Bool -> m a -> m a -> m a
ifElseM b x y = b >>= \ t -> if t then x else y

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM b x = ifElseM b x $ return ()

cond :: (a -> Bool) -> Compiler a -> Compiler a
cond f c = do
    st <- getState
    v  <- c
    if f v
        then return v
        else setState st >> failure ""

addLine :: PseudoLine -> Compiler ()
addLine l = C $ \ env -> Left (addLineToEnv l env, ())

addLines :: Pseudo -> Compiler ()
addLines = loop addLine

addStringLit :: String -> Compiler Int
addStringLit x = C $ \ env -> Left (addStrToEnv x env, envStrLen env)

addFloatLit :: Float -> Compiler Int
addFloatLit x = C $ \ env -> Left (addFloatToEnv x env, envFloatLen env)

getState :: Compiler Env
getState = C $ \ env -> Left (env, env)

setState :: Env -> Compiler ()
setState env = C $ \ _ -> Left (env, ())

getReg :: Compiler Reg
getReg = C $ \ env -> let (reg, newEnv) = envGetReg env in Left (newEnv, fromJust $ reg)

freeRegM :: Maybe Reg -> Compiler ()
freeRegM (Just reg) = freeReg reg
freeRegM Nothing    = C $ \ env -> Left (env, ())

freeReg :: Reg -> Compiler ()
freeReg reg = C $ \ env -> Left (envFreeReg reg env, ())

borrowReg :: Compiler Reg
borrowReg = C $ \ env -> Left (env, fromJust $ fst $ envGetReg env)

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

getFloatFlag :: Compiler Bool
getFloatFlag = C $ \ env -> Left (env, envGetFloat env)

setFloatFlag :: Bool -> Compiler ()
setFloatFlag b = C $ \ env -> Left (envSetFloat b env, ())

----- COMPLIE -----

compileFile :: File -> Compiler ()
compileSymb :: Symb -> Compiler ()
compileStmt :: Stmt -> Compiler ()
compileExpr :: Expr -> Compiler (Maybe Reg, Type)

compileFile (File s) = loop compileSymb s

compileSymb (VarDecl typ name False) = addGlo $ Var name typ Nothing True
compileSymb (VarDecl typ name True)  = addStc $ Var name typ Nothing True
compileSymb (Init typ name expr) = addGlo $ Var name typ (Just $ evaluate expr) True
compileSymb (FunDecl (FuncType retType args) name) = addFun $ Fun name retType args False 0

compileSymb (Func (FuncType retType args) name body) = do
    let numLocals = countLocals body
    addFun (Fun name retType args True numLocals)
    addLines [Label $ underscore ++ name, Push RegEbp, MovReg RegEbp RegEsp]
    when (numLocals > 0) $ addLine $ SubConst RegEsp $ toInteger numLocals
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
        Just ex -> do
            (reg, typ) <- compileExpr (App "=" [Name name, ex])
            freeRegM reg
            return ()
        Nothing -> return ()

compileStmt (If cond st1 st2) = do
    (reg, _) <- compileExpr cond
    id <- addIfId >> getIdString
    addLine $ Cmp $ fromJust reg
    freeRegM reg
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
    addLine $ Cmp $fromJust reg
    freeRegM reg
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
    addLine $ Cmp $ fromJust reg
    freeRegM reg
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
    when (isJust reg && (fromJust reg) /= RegEax) $addLine $ MovReg RegEax $ fromJust reg
    addLine $ Ret id
    freeRegM reg

compileStmt (Expr expr) = compileExpr expr >>= freeRegM . fst >> return ()

compileExpr (Literal s) = do
    reg <- getReg
    LoadLit reg <$> addStringLit s >>= addLine
    return (Just reg, PtrType $ PrimType "char")

compileExpr (Number x) = ifElseM getFloatFlag floatNum intNum where
    intNum = do
        reg <- getReg
        addLine $ Mov reg x
        return (Just reg, fromJust $ getIntType x)
    floatNum = compileExpr (Float $ fromIntegral x)

compileExpr (Float x) = do
    let typ = fromJust $ getFloatType x
    LoadFloat 8 <$> addFloatLit x >>= addLine
    return (Nothing, typ)

compileExpr (Name name) = do
    t <- getVarType name <|> getFunType name
    if typeIsFloat t
        then loadLocFloat t >> return (Nothing, t)
        else do
            if typeIsArray t
                then compileExpr (App "&" [Name name])
                else do
                    r <- getReg
                    loadLoc r <|> loadFun r <|> loadGlo r
                    return (Just r, t)
    where
    loadLoc r = do 
        i <- getVarOffset name
        addLine $ LoadLoc r $ toInteger i
    loadGlo r = getVarType name >> (addLine $ Load r name)
    loadFun r = getFunType name >> (addLine $ Load r $ underscore ++ name)
    loadLocFloat typ = do 
        i <- getVarOffset name
        addLine $ FldLoc i $ getTypeSize typ

compileExpr (App "&" [Name name]) = do
    reg <- getReg
    typ <- ((AddrLoc reg) <$> toInteger <$> (getVarOffset name) >>= addLine >> getVarType name)
        <|> (getVarType name <* (addLine $ Addr reg name))
        <|> (getFunType name <* (addLine $ Addr reg $ underscore ++ name))
        <|> failure "Can only get address of l-value"
    return (Just reg, PtrType typ)

compileExpr (App "+=" [expr1, expr2]) = compileExpr $ App "=" [expr1, App "+" [expr1, expr2]]
compileExpr (App "-=" [expr1, expr2]) = compileExpr $ App "=" [expr1, App "-" [expr1, expr2]]
compileExpr (App "*=" [expr1, expr2]) = compileExpr $ App "=" [expr1, App "*" [expr1, expr2]]
compileExpr (App "/=" [expr1, expr2]) = compileExpr $ App "=" [expr1, App "/" [expr1, expr2]]
    
compileExpr (App "=" [Name name, expr]) = do
    varTyp     <- getVarType name
    (reg, typ) <- if typeIsFloat varTyp then (float varTyp $ compileExpr expr) else compileExpr expr
    if typeIsFloat typ
        then saveLocFloat name typ
        else saveLoc name typ $ fromJust reg
    return (reg, typ)
    where
        saveLocFloat name typ = do
            offset <- getVarOffset name
            addLine (FstLoc offset 8)
        saveLoc name typ reg = (SaveLoc reg <$> toInteger <$> getVarOffset name >>= addLine)
            <|> (addLine $ Save name reg $ toInteger $ getTypeSize typ)
            

compileExpr (App "=" [App "$" [addrExpr], valueExpr]) = do
    (addrReg, ptrTyp)    <- unwrapReg <$> compileExpr addrExpr
    (valueReg, valueTyp) <- unwrapReg <$> compileExpr valueExpr
    addrTyp              <- deref ptrTyp
    tryCast addrTyp valueTyp
    addLine $ SaveToPtr addrReg valueReg $ toInteger $ getTypeSize addrTyp
    freeReg valueReg
    return (Just addrReg, addrTyp)

compileExpr (App "=" [ArrayDeref addrExpr iEx, expr]) = do
    (addrReg, ptrTyp)    <- unwrapReg <$> compileExpr addrExpr
    (valueReg, valueTyp) <- unwrapReg <$> compileExpr expr
    (iReg, iTyp)         <- unwrapReg <$> compileExpr iEx
    addrTyp              <- deref ptrTyp
    tryCast addrTyp valueTyp
    fixPtrOffset iReg addrTyp
    failIf (not $ typeIsInt iTyp) "Array index must be int"
    addLine $ Add addrReg iReg
    addLine $ SaveToPtr addrReg valueReg $ toInteger $ getTypeSize addrTyp
    freeReg valueReg
    freeReg valueReg
    return (Just addrReg, addrTyp)

compileExpr (App "++" [expr]) = compileExpr $ App "=" [expr, App "+" [expr, Number 1]]
compileExpr (App "--" [expr]) = compileExpr $ App "=" [expr, App "-" [expr, Number 1]]

compileExpr (App "+++" [expr]) = do
    (reg, typ) <- unwrapReg <$> (compileExpr $ App "=" [expr, App "+" [expr, Number 1]])
    addLine $ Dec reg
    return (Just reg, typ)

compileExpr (App "---" [expr]) = do
    (reg, typ) <- unwrapReg <$> (compileExpr $ App "=" [expr, App "-" [expr, Number 1]])
    addLine $ Inc reg
    return (Just reg, typ)

compileExpr (App sym [expr]) = do
    (reg, typ) <- unwrapReg <$> compileExpr expr
    let retType = getType sym typ undefined
    failIf (retType == Nothing) "Incompatible type"
    case sym of
        "$" -> addLine $ DeRef reg
        "!" -> addLines [Test reg, Setz reg, AndConst reg 1]
    return (Just reg, fromJust retType)

compileExpr (App sym [expr1, expr2]) = do
    type1 <- getExprType $ compileExpr expr1
    type2 <- getExprType $ compileExpr expr2
    fl    <- getFloatFlag
    if fl || typeIsFloat type1 || typeIsFloat type2
        then do
            let inputType = getInputType sym type1 type2
            failIf (inputType == Nothing) "Incompatible types"
            float (fromJust inputType) $ compileExpr expr2
            float (fromJust inputType) $ compileExpr expr1
            floatExpr sym (fromJust inputType) (fromJust inputType)
        else do
            e1 <- unwrapReg <$> compileExpr expr1
            e2 <- unwrapReg <$> compileExpr expr2
            intExpr sym e1 e2

compileExpr (ArrayDeref ex i) = do
    (reg, typ) <- unwrapReg <$> compileExpr ex
    newTyp <- deref typ
    (iReg, iTyp) <- unwrapReg <$> compileExpr i
    failIf (not $ typeIsInt iTyp) $ "Array index must be integer"
    fixPtrOffset iReg typ
    addLines [Add reg iReg, DeRef reg]
    freeReg iReg
    return (Just reg, newTyp)

compileExpr (Ternary cond expr1 expr2) = do
    (reg, _) <- unwrapReg <$> compileExpr cond
    id <- addIfId >> getIdString
    addLine $ Cmp reg
    freeReg reg
    addLine $ Je $ id ++ ".else"
    (reg2, typ2) <- unwrapReg <$> compileExpr expr1
    addLine $ Jmp $ id ++ ".end"
    addLine $ Label $ id ++ ".else"
    (reg3, typ3) <- unwrapReg <$> compileExpr expr2
    addLine $ MovReg reg2 reg3
    addLine $ Label $ id ++ ".end"
    freeReg reg3
    popId
    return (Just reg2, typ2)

compileExpr c@(Call ex args) = callByName c <|> callByAddr c

callByName (Call (Name name) args) = do
    retType <- funRetType <$> getFun name
    reg     <- borrowReg
    when (reg /= RegEax) $ addLine $ Push RegEax -- freeRegM0
    size <- handleCallArgs args
    _    <- getReg
    addLine $ CallName (underscore ++ name) []
    when (length args > 0) $ addLine $ AddConst RegEsp $ toInteger size
    when (reg /= RegEax) $ addLines [MovReg reg RegEax, Pop RegEax] -- getReg0
    return (Just reg, retType)

callByAddr (Call ex args) = do
    (reg, typ) <- unwrapReg <$> compileExpr ex
    case typ of
        (PtrType (FuncType _ _)) -> return ()
        _ -> failure $ (show typ) ++ "Not a pointer to a function"
    when (reg /= RegEax) $ addLine $ Push RegEax
    size <- handleCallArgs args
    addLine $ CallAddr reg []
    when (length args > 0) $ addLine $ AddConst RegEsp $ toInteger size
    when (reg /= RegEax) $ addLines [MovReg reg RegEax, Pop RegEax]
    return (Just reg, typ)

handleCallArgs :: [Expr] -> Compiler Int
handleCallArgs = foldM (\ a b -> (+a) <$> (pushFloat a b <|> push b)) 0 . reverse where
    pushFloat offset e = do
        ts <- getTypeSize <$> snd <$> isFloat (compileExpr e)
        addLines [SubConst RegEsp (toInteger ts), FstpReg RegEsp offset ts]
        return ts
    push e = do
        (r, t) <- unwrapReg <$> compileExpr e
        addLine $ Push r
        freeReg r
        if getTypeSize t < 4
            then return 4
            else return $ getTypeSize t

fixPtrOffset :: Reg -> Type -> Compiler ()
fixPtrOffset reg1 typ =
    maybe (return ())
        (\ t -> do
            reg2 <- borrowReg
            addLine $ Mov reg2 $ toInteger $ getTypeSize t
            addLine $ Mul reg1 reg2
        )
        $ getPtrType typ

intExpr sym (reg1, type1) (reg2, type2) = do 
    let retType = getType sym type1 type2
    failIf (retType == Nothing) "Incompatible types"
    when (sym == "+" || sym == "-") $ do
        fixPtrOffset reg1 type2
        fixPtrOffset reg2 type1
    case sym of
        "+" -> addLine $ Add reg1 reg2
        "-" -> addLine $ Sub reg1 reg2
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
        "<<" -> addLines [Push RegEcx, MovReg RegEcx reg2, Shl reg1, Pop RegEcx]
        ">>" -> addLines [Push RegEcx, MovReg RegEcx reg2, Shr reg1, Pop RegEcx]
    freeReg reg2
    return (Just reg1, fromJust retType)

floatExpr sym type1 type2 = do
    let retType = getType sym type1 type2
    failIf (retType == Nothing) "Incompatible types"
    if typeIsFloat $ fromJust retType
        then do
            case sym of
                "+"  -> addLine Fadd
                "-"  -> addLine Fsub
                "*"  -> addLine Fmul
                "/"  -> addLine Fdiv
            return (Nothing, fromJust retType)
        else do
            reg <- getReg
            case sym of
                "==" -> addLines [Fcom, FnstswAx, Sahf, Setz reg, AndConst reg 1]
                "!=" -> addLines [Fcom, FnstswAx, Sahf, Setnz reg, AndConst reg 1]
                ">" -> addLines [Fcom, FnstswAx, Sahf, Seta reg, AndConst reg 1]
                "<" -> addLines [Fcom, FnstswAx, Sahf, Setb reg, AndConst reg 1]
                ">=" -> addLines [Fcom, FnstswAx, Sahf, Setae reg, AndConst reg 1]
                "<=" -> addLines [Fcom, FnstswAx, Sahf, Setbe reg, AndConst reg 1]
            return (Just reg, fromJust retType)

getExprType :: Compiler (Maybe Reg, Type) -> Compiler Type
getExprType c = getState >>= \ s -> snd <$> c <* setState s

isFloat :: Compiler (Maybe Reg, Type) -> Compiler (Maybe Reg, Type)
isFloat = cond (typeIsFloat . snd)

isInt :: Compiler (Reg, Type) -> Compiler (Reg, Type)
isInt = cond (typeIsInt . snd)

float :: Type -> Compiler (Maybe Reg, Type) -> Compiler (Maybe Reg, Type)
float t c = do
    setFloatFlag True
    res <- c
    cast t res
    setFloatFlag False
    return res

tryCast :: Type -> Type -> Compiler ()
tryCast l r = failIf (not $ canCast l r)  $ "Cannot autocast from " ++ show r ++ " to " ++ show l

cast :: Type -> (Maybe Reg, Type) -> Compiler (Maybe Reg, Type)
cast targetType (reg, sourceType) = do
    tryCast targetType sourceType
    if typeIsFloat targetType && not (typeIsFloat sourceType)
        then do
            addLine $ Push $ fromJust reg
            addLine $ Fild RegEsp 0 $ getTypeSize sourceType
            addLine $ AddConst RegEsp 4
            return (Nothing, targetType)
        else return (reg, targetType)

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

unwrapReg :: (Maybe Reg, Type) -> (Reg, Type)
unwrapReg ((Just reg), typ) = (reg, typ)
unwrapReg (Nothing, _) = error "Could not unwrap reg"