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
    
    | CallName  String [Reg]
    | CallAddr  Reg [Reg]
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
    | Setnz Reg
    | Setl  Reg
    | Setg  Reg
    | Setle Reg
    | Setge Reg
    | Seta  Reg
    | Setb  Reg
    | Setae Reg
    | Setbe Reg
    
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
    | Fcom
    | FnstswAx
    | Sahf
    | TestFloat Reg
    
    | Fld          String Int -- name size
    | Fst          String Int
    | Fstp         String Int
    | FldLoc       Int Int -- offset size
    | FstLoc       Int Int
    | FstpLoc      Int Int
    | FstpReg      Reg Int Int -- reg offset size
    | Fild         Reg Int Int -- reg offset size
    | LoadFloat    Int Int
    | LoadLocFloat Int Int
    
    deriving (Show)

type Pseudo = [PseudoLine]
type Lit = String