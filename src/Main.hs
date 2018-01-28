{-
 - TODO
 - 
 - Register handling
 - Void
 - Break / Continue
 - Floats
 - 
 -}

import Data.List
import System.Environment

import Tokenize
import Parse
import Compile
import Asm
import Translator

--- COMPILE

compile :: String -> Asm
compile file = (uncurry toAsm) $ runCompiler $ compileFile $ translate $ parse $ tokenize file

--- MAIN

--file :: File
--file = File [FunDecl (FuncType (PrimType "int") [(PtrType $ PrimType "char", "output")]) "printf", Func (FuncType (PrimType "int") []) "main" [Expr $ Call (Name "printf") [Literal "hey"]]]

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 2
        then do
            file <- readFile $ (args !! 0)
            writeFile (args !! 1) $ intercalate "\n" $ compile file
        else
            putStrLn $ intercalate "\n" $ compile $ (head args)