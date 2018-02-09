module Asm ( Asm, toAsm ) where

import Data.List
import Data.Maybe

import Pseudo
import Scope
import Type
import Reg

type AsmLine = String
type Asm = [AsmLine]


toAsm :: Pseudo -> Scope -> Asm
toAsm r scope@(Scope gs ss _ _ fs) = toAsmExtern fs ++ toAsmGlobals fs ++
                               ["section .data"] ++ (map toAsmDataLine $ gs ++ ss) ++
                               ["section .rodata", "?strings:"] ++ (map toAsmLitLine lits) ++
                               ["section .text"] ++ asm
    where
        (asm, lits) = toAsmLines $ map (retNumLocals scope) r

toAsmGlobals :: [Fun] -> Asm
toAsmGlobals funs = map (\ f -> "global _" ++ funName f) (filter funIsDef funs)

toAsmExtern :: [Fun] -> Asm
toAsmExtern funs = map (\ f -> "extern _" ++ funName f) (filter (\f -> (not $ funIsDef f) && (not $ any (\f2 -> funName f == funName f2 && funIsDef f2) funs)) funs)

toAsmDataLine :: Var -> AsmLine
toAsmDataLine (Var n (ArrayType t i) v _) = n ++ ": resw " ++ show i
toAsmDataLine (Var n t v _) = n ++ ": " ++ (getSizeWordData $ getTypeSize t) ++ " " ++
    case v of
        Nothing -> "0"
        Just (Integer x) -> show x
        Just (String x) -> "$ + 4" ++ "\ndb '" ++ x ++ "', 0"

toAsmLitLine :: Lit -> String
toAsmLitLine l = "db `" ++ l ++ "`, 0"

toAsmLines :: Pseudo -> (Asm, [Lit])
toAsmLines rtl = toAsmLinesLoop rtl 0 [] [] where
    toAsmLinesLoop [] _ asm lits = (asm, lits)
    toAsmLinesLoop (x:xs) i asm lits = toAsmLinesLoop xs (i+1) (asm++lines) (lits++newLit)
        where
            (lines, newLit) = toAsmLine x i lits

toAsmLine :: PseudoLine -> Integer -> [Lit] -> ([AsmLine], [Lit])
toAsmLine (Add reg1 reg2) _ _        = (["add " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Sub reg1 reg2) _ _        = (["sub " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Mul reg1 reg2) _ _        = (["imul " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Div reg1 reg2) _ _        = (["div " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Mov reg i) _ _            = (["mov " ++ getReg reg  ++ ", " ++ show i], [])
toAsmLine (Load reg name) _ _        = (["mov " ++ getReg reg  ++ ", [" ++ name ++ "]"], [])
toAsmLine (Save name reg size) _ _   = (["mov " ++ (getSizeWord size) ++ " [" ++ name ++ "], " ++ getPartialReg size reg], [])
toAsmLine (SaveToPtr reg1 reg2 size) _ _ = (["mov " ++ (getSizeWord size) ++ " [" ++ getReg reg1 ++ "], " ++ getPartialReg size reg2], [])
toAsmLine (Label name) _ _           = ([name ++ ":"], [])
toAsmLine (Cmp reg) _ _              = (["cmp " ++ getReg reg ++ ", 0"], [])
toAsmLine (Jmp label) _ _            = (["jmp " ++ label], [])
toAsmLine (Je label) _ _             = (["je " ++ label], [])
toAsmLine (Jne label) _ _            = (["jne " ++ label], [])
toAsmLine (Jle label) _ _            = (["jle " ++ label], [])
toAsmLine (Jl label) _ _             = (["jl " ++ label], [])
toAsmLine (CallName name args _) _ _ = (["call " ++ name], [])
toAsmLine (CallAddr addr args _) _ _ = (["call " ++ getReg addr], [])
toAsmLine (DeRef reg) _ _            = (["mov " ++ getReg reg ++ ", [" ++ getReg reg ++ "]"], [])
toAsmLine (Ret i) _ _                = ((if (read i) > 0 then ["add " ++ getReg reg_esp ++ ", " ++ i] else []) ++ ["pop " ++ getReg reg_ebp, "ret"], [])
toAsmLine (Push reg) _ _             = (["push " ++ getReg reg], [])
toAsmLine (Pop reg) _ _              = (["pop " ++ getReg reg], [])
toAsmLine (LoadLoc reg offset) _ _   = (["mov " ++ getReg reg ++ ", [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"], [])
toAsmLine (SaveLoc offset reg) _ _   = (["mov [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "], " ++ getReg reg], [])
toAsmLine (AddConst reg int)  _ _    = (["add " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (SubConst reg int) _ _     = (["sub " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (MulConst reg int) _ _     = (["mul " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (DivConst reg int) _ _     = (["div " ++ getReg reg ++ ", " ++ show int], [])
toAsmLine (LoadLit reg l) _ ls       = (["mov " ++ getReg reg ++ ", ?strings + " ++ show (litsGetSize ls)], [l])
toAsmLine (MovReg reg1 reg2) _ _     = (["mov " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Addr reg name) _ _        = (["mov " ++ getReg reg ++ ", " ++ name], [])
toAsmLine (AddrLoc reg offset) _ _   = (["lea " ++ getReg reg ++ ", [" ++ getReg reg_ebp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"], [])
toAsmLine (Test reg) _ _             = (["test " ++ getReg reg ++ ", " ++ getReg reg], [])
toAsmLine (Setz reg) _ _             = (["setz " ++ getRegLower reg], [])
toAsmLine (Setl reg) _ _             = (["setl " ++ getRegLower reg], [])
toAsmLine (Setg reg) _ _             = (["setg " ++ getRegLower reg], [])
toAsmLine (Setle reg) _ _            = (["setle " ++ getRegLower reg], [])
toAsmLine (Setge reg) _ _            = (["setge " ++ getRegLower reg], [])
toAsmLine (AndConst reg i) _ _       = (["and " ++ getReg reg ++ ", " ++ show i], [])
toAsmLine (Or reg1 reg2) _ _         = (["or " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Xor reg1 reg2) _ _        = (["xor " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (And reg1 reg2) _ _        = (["and " ++ getReg reg1 ++ ", " ++ getReg reg2], [])
toAsmLine (Shl reg) _ _              = (["shl " ++ getReg reg ++ ", cl"], [])
toAsmLine (Shr reg) _ _              = (["shr " ++ getReg reg ++ ", cl"], [])
toAsmLine (Inc reg) _ _              = (["inc " ++ getReg reg], [])
toAsmLine (Dec reg) _ _              = (["dec " ++ getReg reg], [])

retNumLocals :: Scope -> PseudoLine -> PseudoLine
retNumLocals s (Ret n) = Ret $ show $ (funcGetNumLoc s n)
retNumLocals _ a = a

litsGetSize :: [Lit] -> Int
litsGetSize list = foldr (\ l s -> s + (length l) + 1) 0 list

funcGetNumLoc :: Scope -> String -> Int
funcGetNumLoc (Scope _ _ _ _ fs) n = numLocals $ fromJust $ find (\f -> (funName f) == n) fs

getReg :: Reg -> String
getReg (-3) = "eax"
getReg (-2) = "ebp"
getReg (-1) = "esp"
getReg 0 = "eax"
getReg 1 = "ebx"
getReg 2 = "ecx"
getReg 3 = "edx"

getPartialReg :: Integer -> Reg -> String
getPartialReg 1 r = (getReg r) !! 1 : "l"
getPartialReg 2 r = (getReg r) !! 1 : "x"
getPartialReg 4 r = getReg r

getRegLower :: Reg -> String
getRegLower = getPartialReg 1

getSizeWordData :: Int -> String
getSizeWordData 1 = "db"
getSizeWordData 2 = "dw"
getSizeWordData 4 = "dd"

getSizeWord :: Integer -> String
getSizeWord 1 = "byte"
getSizeWord 2 = "word"
getSizeWord 4 = "dword"