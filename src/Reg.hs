module Reg ( Reg (..), Regs, regGet, regTake, regFree, allRegs ) where

import Data.List

data Reg = RegEsp | RegEbp | RegEax | RegEbx | RegEcx | RegEdx deriving (Show, Eq)

type Regs = [Reg]

allRegs :: Regs
allRegs = [RegEax, RegEbx, RegEcx, RegEdx]

regGet :: Regs -> (Maybe Reg, Regs)
regGet rs = case find ( \ r -> regIsTaken r rs) allRegs of
    (Just r) -> (Just r, filter (/=r) rs)
    Nothing  -> (Nothing, rs)

regTake :: Reg -> Regs -> Maybe Regs
regTake r rs = if regIsTaken r rs
    then Just $ filter (/=r) rs
    else Nothing

regFree :: Reg -> Regs -> Regs
regFree x xs
    | elem x xs = error $ "Reg " ++ (show x) ++ " already free"
    | otherwise = x:xs

regIsTaken :: Reg -> Regs -> Bool
regIsTaken = elem