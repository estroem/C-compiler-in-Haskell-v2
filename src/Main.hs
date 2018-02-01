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
import ParseMonad

--- COMPILE

run :: String -> Asm
run = (uncurry toAsm) . runCompiler . compileFile . parseMonad . tokenize

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 2
        then do
            file <- readFile $ (args !! 0)
            writeFile (args !! 1) $ intercalate "\n" $ run file
        else
            putStrLn $ intercalate "\n" $ run $ (head args)