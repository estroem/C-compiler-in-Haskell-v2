{-
 - TODO
 - 
 - Register handling
 - Floats
 - 
 -}

import Data.List
import System.Environment

import Preprocess
import Tokenize
import Parse
import Compile
import Asm

--- COMPILE

run :: String -> Asm
run = (uncurry toAsm) . compile . parse . tokenize

--- MAIN

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 2
        then do
            file <- readFile (args !! 0) >>= preprocess
 --           writeFile (args !! 1) $ show $ parse $ tokenize $ file
            writeFile (args !! 1) $ intercalate "\n" $ run file
        else
            putStrLn $ intercalate "\n" $ run $ (head args)