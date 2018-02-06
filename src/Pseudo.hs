module Pseudo ( Pseudo, PseudoLine (..), Lit ) where

import Reg

data PseudoLine = Add Reg Reg | Sub Reg Reg | Mul Reg Reg | Div Reg Reg | Mov Reg Integer
             | Load Reg String | Save String Reg Integer | SaveToPtr Reg Reg Integer | Label String
             | Cmp Reg | Jmp String | Je String | Jne String | Jle String | Jl String
             | CallName String [Reg] Reg | CallAddr Reg [Reg] Reg | DeRef Reg
             | FuncStart String | FuncEnd String | Ret String | Push Reg | LoadLoc Reg Integer
             | SaveLoc Integer Reg | Pop Reg | AddConst Reg Integer | SubConst Reg Integer
             | MulConst Reg Integer | DivConst Reg Integer | LoadLit Reg String | MovReg Reg Reg
             | Addr Reg String | AddrLoc Reg Integer | Test Reg | Setz Reg | Setl Reg | Setg Reg
             | Setle Reg | Setge Reg | AndConst Reg Integer | And Reg Reg | Xor Reg Reg | Or Reg Reg
             | Shl Reg Reg | Shr Reg Reg
    deriving (Show)

type Pseudo = [PseudoLine]
type Lit = String