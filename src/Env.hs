module Env where

import Pseudo
import Reg
import Scope
import qualified Id

data Env = E {
    envAsm    :: Pseudo,
    envFreeRegs :: Regs,
    envScope  :: Scope,
    envId     :: Id.Id,
    envFF     :: Bool,
    envStrs   :: [Lit],
    envFloats :: [Float]
} deriving (Show)

emptyEnv :: Env
emptyEnv = E [] allRegs emptyScope ([], 0) False [] []

envGetResult :: Env -> (Pseudo, Scope, [Lit], [Float])
envGetResult e = (envAsm e, envScope e, envStrs e, envFloats e)

envGetAsm :: Env -> Pseudo
envGetAsm = envAsm

envGetReg :: Env -> (Maybe Reg, Env)
envGetReg e = let (r, rs) = regGet $ envFreeRegs e in (r, e { envFreeRegs = rs })

envFreeReg :: Reg -> Env -> Env
envFreeReg r e = e { envFreeRegs = (regFree r $ envFreeRegs e) }

addLineToEnv :: PseudoLine -> Env -> Env
addLineToEnv line e = e { envAsm = envAsm e ++ [line] }

addStrToEnv :: String -> Env -> Env
addStrToEnv str e = e { envStrs = str : envStrs e }

envStrLen :: Env -> Int
envStrLen = foldr (\ a b -> b + strLength a + 1) 0 . envStrs

addFloatToEnv :: Float -> Env -> Env
addFloatToEnv float e = e { envFloats = float : envFloats e }

envFloatLen :: Env -> Int
envFloatLen = (*8) . length . envFloats

envGetScope :: Env -> Scope
envGetScope = envScope

envSetScope :: Scope -> Env -> Env
envSetScope sc e = e { envScope = sc }

envGetId :: Env -> Id.Id
envGetId = envId

envSetId :: Id.Id -> Env -> Env
envSetId id e = e { envId = id }

envGetFloat :: Env -> Bool
envGetFloat = envFF

envSetFloat :: Bool -> Env -> Env
envSetFloat f e = e { envFF = f }

strLength :: String -> Int
strLength [] = 0
strLength ('\\':xs) = strLength xs
strLength (x:xs) = 1 + strLength xs