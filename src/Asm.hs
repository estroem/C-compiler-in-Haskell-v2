module Asm ( Asm, toAsm ) where

import Data.List
import Data.Maybe

import Pseudo
import Scope
import Type
import Reg

type AsmLine = String
type Asm = [AsmLine]


underscore = ""

toAsm :: Pseudo -> Scope -> [Lit] -> [Float] -> Asm
toAsm r scope@(Scope gs ss _ _ fs) lits floats = toAsmExtern fs ++ toAsmGlobals fs ++
                               ["section .data"] ++ (map toAsmDataLine $ gs ++ ss) ++
                               ["section .rodata", "?strings:"] ++ (map toAsmLitLine $ reverse lits) ++
                               ["?floats:"] ++ (map toAsmFloatLine $ reverse floats) ++
                               ["section .text"] ++ asm
    where
        asm = toAsmLines $ map (retNumLocals scope) r

toAsmGlobals :: [Fun] -> Asm
toAsmGlobals funs = map (\ f -> "global " ++ underscore ++ funName f) (filter funIsDef funs)

toAsmExtern :: [Fun] -> Asm
toAsmExtern funs = map (\ f -> "extern " ++ underscore ++ funName f) (filter (\f -> (not $ funIsDef f) && (not $ any (\f2 -> funName f == funName f2 && funIsDef f2) funs)) funs)

toAsmDataLine :: Var -> AsmLine
toAsmDataLine (Var n (ArrayType t i) v _) = n ++ ": resw " ++ show i
toAsmDataLine (Var n t v _) = n ++ ": " ++ (getSizeWordData $ getTypeSize t) ++ " " ++
    case v of
        Nothing -> "0"
        Just (Integer x) -> show x
        Just (String x) -> "$ + 4" ++ "\ndb '" ++ x ++ "', 0"

toAsmLitLine :: Lit -> String
toAsmLitLine l = "db `" ++ l ++ "`, 0"

toAsmFloatLine :: Float -> String
toAsmFloatLine f = "dq " ++ show f

toAsmLines :: Pseudo -> Asm
toAsmLines rtl = toAsmLinesLoop rtl [] where
    toAsmLinesLoop [] asm = asm
    toAsmLinesLoop (x:xs) asm = toAsmLinesLoop xs (asm++(toAsmLine x))

toAsmLine :: PseudoLine -> [AsmLine]
toAsmLine (Add reg1 reg2)            = ["add " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Sub reg1 reg2)            = ["sub " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Mul reg1 reg2)            = ["imul " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Div reg1 reg2)            = ["div " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Mov reg i)                = ["mov " ++ getReg reg  ++ ", " ++ show i]
toAsmLine (Load reg name)            = ["mov " ++ getReg reg  ++ ", [" ++ name ++ "]"]
toAsmLine (Save name reg size)       = ["mov " ++ (getSizeWord size) ++ " [" ++ name ++ "], " ++ getPartialReg size reg]
toAsmLine (SaveToPtr reg1 reg2 size) = ["mov " ++ (getSizeWord size) ++ " [" ++ getReg reg1 ++ "], " ++ getPartialReg size reg2]
toAsmLine (Label name)               = [name ++ ":"]
toAsmLine (Cmp reg)                  = ["cmp " ++ getReg reg ++ ", 0"]
toAsmLine (Jmp label)                = ["jmp " ++ label]
toAsmLine (Je label)                 = ["je " ++ label]
toAsmLine (Jne label)                = ["jne " ++ label]
toAsmLine (Jle label)                = ["jle " ++ label]
toAsmLine (Jl label)                 = ["jl " ++ label]
toAsmLine (CallName name args)       = ["call " ++ name]
toAsmLine (CallAddr addr args)       = ["call " ++ getReg addr]
toAsmLine (DeRef reg)                = ["mov " ++ getReg reg ++ ", [" ++ getReg reg ++ "]"]
toAsmLine (Ret i)                    = (if (read i) > 0 then ["add " ++ getReg RegEsp ++ ", " ++ i] else []) ++ ["pop " ++ getReg RegEbp, "ret"]
toAsmLine (Push reg)                 = ["push " ++ getReg reg]
toAsmLine (Pop reg)                  = ["pop " ++ getReg reg]
toAsmLine (LoadLoc reg offset)       = ["mov " ++ getReg reg ++ ", [" ++ getReg RegEbp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"]
toAsmLine (SaveLoc reg offset)       = ["mov [" ++ getReg RegEbp ++ (if offset > 0 then "+" else "") ++ show offset ++ "], " ++ getReg reg]
toAsmLine (AddConst reg int)         = ["add " ++ getReg reg ++ ", " ++ show int]
toAsmLine (SubConst reg int)         = ["sub " ++ getReg reg ++ ", " ++ show int]
toAsmLine (MulConst reg int)         = ["mul " ++ getReg reg ++ ", " ++ show int]
toAsmLine (DivConst reg int)         = ["div " ++ getReg reg ++ ", " ++ show int]
toAsmLine (LoadLit reg i)            = ["mov " ++ getReg reg ++ ", ?strings + " ++ show i]
toAsmLine (MovReg reg1 reg2)         = ["mov " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Addr reg name)            = ["mov " ++ getReg reg ++ ", " ++ name]
toAsmLine (AddrLoc reg offset)       = ["lea " ++ getReg reg ++ ", [" ++ getReg RegEbp ++ (if offset > 0 then "+" else "") ++ show offset ++ "]"]
toAsmLine (Test reg)                 = ["test " ++ getReg reg ++ ", " ++ getReg reg]
toAsmLine (Setz reg)                 = ["setz " ++ getRegLower reg]
toAsmLine (Setl reg)                 = ["setl " ++ getRegLower reg]
toAsmLine (Setg reg)                 = ["setg " ++ getRegLower reg]
toAsmLine (Setle reg)                = ["setle " ++ getRegLower reg]
toAsmLine (Setge reg)                = ["setge " ++ getRegLower reg]
toAsmLine (AndConst reg i)           = ["and " ++ getReg reg ++ ", " ++ show i]
toAsmLine (Or reg1 reg2)             = ["or " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Xor reg1 reg2)            = ["xor " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (And reg1 reg2)            = ["and " ++ getReg reg1 ++ ", " ++ getReg reg2]
toAsmLine (Shl reg)                  = ["shl " ++ getReg reg ++ ", cl"]
toAsmLine (Shr reg)                  = ["shr " ++ getReg reg ++ ", cl"]
toAsmLine (Inc reg)                  = ["inc " ++ getReg reg]
toAsmLine (Dec reg)                  = ["dec " ++ getReg reg]
toAsmLine (Fadd)                     = ["faddp"]
toAsmLine (Fsub)                     = ["fsubp"]
toAsmLine (Fmul)                     = ["fmulp"]
toAsmLine (Fdiv)                     = ["fdivp"]
toAsmLine (Fld name size)            = ["fld " ++ getSizeWord (toInteger size) ++ " [" ++ name ++ "]"]
toAsmLine (Fst name size)            = ["fst " ++ getSizeWord (toInteger size) ++ " [" ++ name ++ "]"]
toAsmLine (Fstp name size)           = ["fstp " ++ getSizeWord (toInteger size) ++ " [" ++ name ++ "]"]
toAsmLine (FldLoc offset size)       = ["fld " ++ getSizeWord (toInteger size) ++ " [" ++ getReg RegEbp ++ getOffsetAsString offset ++ "]"]
toAsmLine (FstLoc offset size)       = ["fst " ++ getSizeWord (toInteger size) ++ " [" ++ getReg RegEbp ++ getOffsetAsString offset ++ "]"]
toAsmLine (FstpLoc offset size)      = ["fstp " ++ getSizeWord (toInteger size) ++ " [" ++ getReg RegEbp ++ getOffsetAsString offset ++ "]"]
toAsmLine (FstpReg reg offset size)  = ["fstp " ++ getSizeWord (toInteger size) ++ " [" ++ getReg reg ++ getOffsetAsString offset ++ "]"]
toAsmLine (Fild reg offset size)     = ["fild " ++ getSizeWord (toInteger size) ++ " [" ++ getReg reg ++ getOffsetAsString offset ++ "]"]
toAsmLine (LoadFloat size i)         = ["fld " ++ getSizeWord (toInteger size) ++ " [?floats + " ++ show i ++ "]"]

getOffsetAsString :: Int -> String
getOffsetAsString off = if off /= 0 then ((if off > 0 then "+" else "") ++ show off) else ""

retNumLocals :: Scope -> PseudoLine -> PseudoLine
retNumLocals s (Ret n) = Ret $ show $ (funcGetNumLoc s n)
retNumLocals _ a = a

litsGetSize :: [Lit] -> Int
litsGetSize list = foldr (\ l s -> s + (length l) + 1) 0 list

funcGetNumLoc :: Scope -> String -> Int
funcGetNumLoc (Scope _ _ _ _ fs) n = numLocals $ fromJust $ find (\f -> (funName f) == n) fs

getReg :: Reg -> String
getReg RegEbp = "ebp"
getReg RegEsp = "esp"
getReg RegEax = "eax"
getReg RegEbx = "ebx"
getReg RegEcx = "ecx"
getReg RegEdx = "edx"

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
getSizeWordData 8 = "dq"

getSizeWord :: Integer -> String
getSizeWord 1 = "byte"
getSizeWord 2 = "word"
getSizeWord 4 = "dword"
getSizeWord 8 = "qword"