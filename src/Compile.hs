module Compile ( runCompiler, compileFile, compile ) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative

import Scope
import Reg
import Type
import Pseudo
import Asm
import NewAst

compile :: File -> (Pseudo, Scope)
compile = runCompiler . compileFile

underscore = "_"
reg_eax = 0

----- ENVIRONMENT -----

type Env = (Pseudo, Reg, Scope)
type Id = Int

startReg = 0
emptyEnv = ([], startReg, emptyScope)

envGetAsm :: Env -> Pseudo
envGetAsm (s, _, _) = s

getRegFromEnv :: Env -> Reg
getRegFromEnv (_, r, _) = r

envFreeReg :: Env -> Env
envFreeReg (a, r, s) = (a, r - 1, s)

addLineToEnv :: PseudoLine -> Env -> Env
addLineToEnv l (xs, r, s) = (xs++[l], r, s)

nextReg :: Env -> Env
nextReg (env, r, s) = (env, r + 1, s)

envGetScope :: Env -> Scope
envGetScope (_, _, s) = s

envSetScope :: Scope -> Env -> Env
envSetScope s (a, r, _) = (a, r, s)

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
    getFun name >>= return . funRetType

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

addLoc :: Var -> Compiler ()
addLoc var = getScope >>= setScope . flip scopeAddLoc var

addFun :: Fun -> Compiler ()
addFun fun = getScope >>= setScope . flip scopeAddFun fun

getScope :: Compiler Scope
getScope = C $ \ env -> Left (env, envGetScope env)

setScope :: Scope -> Compiler ()
setScope s = C $ \ env -> Left (envSetScope s env, ())

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
    sc <- getScope
    loop compileStmt body
    setScope sc
    addLine $ Ret name

compileStmt (Block body) = do
    sc <- getScope
    loop compileStmt body
    setScope sc

compileStmt (LocVar typ name _ e) = do
    addLoc $ Var name typ Nothing True
    case e of
        Just ex -> compileExpr (App "=" [Name name, ex]) >> return ()
        Nothing -> return ()

compileStmt (If cond st1 st2) = do
    (reg, _) <- compileExpr cond
    addLine $ Cmp reg
    addLine $ Je "else"
    compileStmt st1
    addLine $ Jmp "end"
    addLine $ Label "else"
    compileStmt st2
    addLine $ Label "end"

compileStmt Nop = return ()

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
    reg <- getReg
    (do
        i <- getVarOffset name
        addLine $ LoadLoc reg $ toInteger i
     ) <|> (do
        getVarType name <|> getFunType name
        addLine $ Load reg name
     )
    typ <- getVarType name <|> getFunType name
    return (reg, typ)

compileExpr (App "+" [expr1, expr2]) = do
    (reg1, type1) <- compileExpr expr1
    (reg2, type2) <- compileExpr expr2
    let retType = getType "+" type1 type2
    failIf (not $ isJust retType) "Incompatible types"
    fixPtrOffset reg1 type1
    fixPtrOffset reg2 type2
    addLine $ Add reg1 reg2
    freeReg
    return (reg1, fromJust retType)

compileExpr (App "=" [Name name, expr]) = do
    (reg, typ) <- compileExpr expr
    varTyp <- getVarType name
    failIf (getType "=" varTyp typ == Nothing) "Incompatible types"
    (do
        i <- toInteger <$> getVarOffset name
        addLine $ SaveLoc i reg
     ) <|> (do
        addLine $ Save name reg $ toInteger $ getTypeSize varTyp
     )
    return (reg, varTyp)

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
        "==" -> addLines [Sub reg1 reg2, Setz reg1, AddConst reg1 1]
        "!=" -> addLine $ Sub reg1 reg2
    freeReg
    return (reg1, fromJust retType)
    
compileExpr (Call (Name name) args) = do
    retType <- getFunType name
    reg <- getReg
    if reg /= reg_eax
        then addLine $ Push reg_eax
        else return ()
    handleCallArgs args
    addLine $ CallName (underscore ++ name) [] 0
    addLine $ AddConst reg_esp $ toInteger $ length args * 4
    if reg /= reg_eax
        then do
            addLine $ MovReg reg reg_eax
            addLine $ Pop reg_eax
        else return ()
    return (reg, retType)

handleCallArgs :: [Expr] -> Compiler ()
handleCallArgs = loop $ \ x -> do
    (reg, _) <- compileExpr x
    addLine $ Push reg
    freeReg
    
fixPtrOffset :: Reg -> Type -> Compiler ()
fixPtrOffset reg1 typ =
    maybe (return ())
        (\ t -> do
            reg2 <- borrowReg
            addLine $ Mov reg2 $ toInteger $ getTypeSize t
            addLine $ Mul reg1 reg2
        )
        $ getPtrType typ

countLocals :: [Stmt] -> Int
countLocals [] = 0
countLocals ((LocVar typ _ False _):xs) = getTypeSize typ + countLocals xs
countLocals ((Block e):xs) = countLocals e + countLocals xs
countLocals ((If _ e1 e2):xs) = countLocals [e1] + countLocals [e2] + countLocals xs
countLocals (_:xs) = countLocals xs

evaluate :: Expr -> Value
evaluate (Number x) = Integer $ fromInteger x
evaluate (Literal s) = String s