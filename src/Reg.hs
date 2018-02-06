module Reg ( Reg, Regs, reg_esp, reg_ebp, reg_eax, reg_ecx, regGet, regTake, regFree ) where

type Reg = Int
type Regs = [Reg]

reg_esp :: Int
reg_ebp :: Int
reg_eax :: Int
reg_ecx :: Int
reg_esp = -1
reg_ebp = -2
reg_eax = 0
reg_ecx = 2

allRegs :: Regs
allRegs = [0, 1, 2, 3]

regGet :: Regs -> Reg
regGet [] = error "No regs available"
regGet (x:xs) = x

regTake :: Reg -> Regs -> Regs
regTake x xs = a ++ tail b
    where (a, b) = span (/=x) xs

regFree :: Reg -> Regs -> Regs
regFree x xs
    | elem x xs = error "Reg already free"
    | otherwise = x:xs

regIsTaken :: Reg -> Regs -> Bool
regIsTaken = elem