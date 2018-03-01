module Pseudo ( Pseudo, PseudoLine (..), Lit ) where

import Reg

data PseudoLine =
      Add Reg Reg
    | Sub Reg Reg
    | Mul Reg Reg
    | Div Reg Reg
    
    | Mov    Reg Integer
    | MovReg Reg Reg
    
    | Load      Reg String
    | Save      String Reg Integer
    | SaveToPtr Reg Reg Integer
    | LoadLoc   Reg Integer
    | SaveLoc   Reg Integer
    
    | Label String
    | Cmp   Reg
    | Jmp   String
    | Je    String
    | Jne   String
    | Jle   String
    | Jl    String
    
    | CallName  String [Reg] Reg
    | CallAddr  Reg [Reg] Reg
    | FuncStart String
    | FuncEnd   String
    | Ret       String
    | Push      Reg
    | Pop       Reg
    
    | AddConst Reg Integer
    | SubConst Reg Integer
    | MulConst Reg Integer
    | DivConst Reg Integer
    
    | LoadLit Reg Int
    
    | DeRef   Reg
    | Addr    Reg String
    | AddrLoc Reg Integer
    
    | Test  Reg
    | Setz  Reg
    | Setl  Reg
    | Setg  Reg
    | Setle Reg
    | Setge Reg
    
    | And      Reg Reg
    | AndConst Reg Integer 
    | Xor      Reg Reg
    | Or       Reg Reg
    | Shl      Reg
    | Shr      Reg
    
    | Inc Reg
    | Dec Reg
    
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Feq
    
    | FldReg       Reg Int
    | FstReg       Reg Int
    | FloatLit     Int Int
    | LoadFloat    Int String
    | LoadLocFloat Int Int
    
    deriving (Show)

type Pseudo = [PseudoLine]
type Lit = String