module Env where

import Pseudo
import Reg
import Scope
import qualified Id

data Env = E {
    envAsm    :: Pseudo,
    envReg    :: Reg,
    envScope  :: Scope,
    envId     :: Id.Id,
    envFF     :: Bool,
    envStrs   :: [Lit],
    envFloats :: [Float]
} deriving (Show)

startReg = 0

emptyEnv :: Env
emptyEnv = E [] startReg emptyScope ([], 0) False [] []

envGetResult :: Env -> (Pseudo, Scope, [Lit], [Float])
envGetResult e = (envAsm e, envScope e, envStrs e, envFloats e)

envGetAsm :: Env -> Pseudo
envGetAsm = envAsm

getRegFromEnv :: Env -> Reg
getRegFromEnv = envReg

nextReg :: Env -> Env
nextReg e = e { envReg = envReg e + 1 }

envFreeReg :: Env -> Env
envFreeReg e = if envReg e == 0 then error "Trying to free reg, but all are free already" else e { envReg = envReg e - 1 }

addLineToEnv :: PseudoLine -> Env -> Env
addLineToEnv line e = e { envAsm = envAsm e ++ [line] }

addStrToEnv :: String -> Env -> Env
addStrToEnv str e = e { envStrs = str : envStrs e }

envStrLen :: Env -> Int
envStrLen = foldr (\ a b -> b + length a + 1) 0 . envStrs

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