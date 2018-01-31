module ParseMonad ( parse ) where

import Data.Char

import NewAst
import Type

data Parser a = P (String -> Maybe (String, a))

instance Functor Parser where
    fmap f (P p) = P $ \ inp -> case p inp of
        Just (inp', x) -> Just (inp', f x)
        Nothing -> Nothing
{-
instance Applicative Parser where
    pure x = P $ \ inp -> Just (inp, x)
    (<*>) (P a) (P b) = P $ \ inp -> case a inp of
        Just (inp', f) -> case b inp' of
            Just (inp'', x) -> Just (inp'', f x)
            Nothing -> Nothing
        Nothing -> Nothing
-}
instance Monad Parser where
    return x = P $ \ inp -> Just (inp, x)
    (>>=) (P a) f = P $ \ inp -> case a inp of
        Just (inp', x) -> let (P b) = (f x) in b inp'
        Nothing -> Nothing

failure :: Parser a
failure = P $ \ _ -> Nothing

parse :: Parser a -> String -> a
parse (P p) inp = maybe (error "fail") snd $ p inp

getState :: Parser String
getState = P $ \ inp -> Just (inp, inp)

setState :: String -> Parser ()
setState s = P $ \ inp -> Just (s, ())

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (P a) (P b) = P $ \ inp -> maybe (b inp) Just $ a inp

many :: Parser a -> Parser [a]
many p = many1 p <|> return []
    
many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return $ x:xs

sep :: Parser a -> Parser b -> Parser [b]
sep s p = sep1 s p <|> return []

sep1 :: Parser a -> Parser b -> Parser [b]
sep1 s p = do
    f <- p
    r <- many (s >> p)
    return $ f:r

file :: Parser File
file = ws >> sep ws symb >>= return . File

symb :: Parser Symb
symb = do
    r <- (do
            r' <- (initVar <|> gloVar <|> func)
            ws >> semi
            return r'
        ) <|> funcDecl
    return r

gloVar :: Parser Symb
gloVar = do
    t <- typ
    ws
    n <- identifier
    return $ VarDecl t n False

initVar :: Parser Symb
initVar = do
    (VarDecl t n _) <- gloVar
    ws
    char '='
    ws
    e <- expr
    return $ Init t n e

funcDecl :: Parser Symb
funcDecl = do
    t <- typ
    n <- identifier
    ws
    args <- parens $ sep (ws >> char ',' >> ws) $ do
        t' <- typ
        n' <- identifier
        return (t', n')
    return $ FunDecl t n

func :: Parser Symb
func = do
    (FunDecl t n) <- funcDecl
    ws
    b <- block
    return $ Func t n [b]

block :: Parser Stmt
block = ws >> (braces $ many stmt >>= return . Block)

stmt :: Parser Stmt
stmt = ws >> (block <|> if' <|> (gloVar >>= \ (VarDecl t n _) -> return $ LocVar t n False Nothing) <|> (expr >>= return . Expr))

if' :: Parser Stmt
if' = do
    string "if"
    c <- parens expr
    s1 <- stmt
    string "else"
    s2 <- stmt
    return $ If c s1 s2

expr :: Parser Expr
expr = number <|> name <|> app

semi :: Parser ()
semi = char ';' >> return ()

parens :: Parser a -> Parser a
parens p = do
    char '('
    r <- p
    char ')'
    return r

braces :: Parser a -> Parser a
braces p = do
    char '{'
    r <- p
    char '}'
    return r
    
literal :: Parser Expr
literal = do
    char '"'
    s <- many $ cond (/='"') single
    char '"'
    return $ Literal $ '"':s

call :: Parser Expr
call = do
    e <- expr
    a <- parens $ sep (char ',') expr
    return $ Call e a

app :: Parser Expr
app = do
    x <- expr
    s <- single
    y <- expr
    return $ App [s] [x, y]

name :: Parser Expr
name = ws >> identifier >>= return . Name

number :: Parser Expr
number = ws >> many digit >>= return . Number . read
    
identifier :: Parser String
identifier = ws >> (many $ cond isAlpha single)
    
keyword :: String -> Parser String
keyword s = ws >> string s

typ :: Parser Type
typ = ws >> string "int" >> (return $ PrimType "int")

digit :: Parser Char
digit = cond isDigit single

single :: Parser Char
single = P $ \ inp ->
    if inp /= ""
        then Just (tail inp, head inp)
        else Nothing

cond :: (a -> Bool) -> Parser a -> Parser a
cond f p = do
    st <- getState
    x <- p
    if f x
        then return x
        else do
            setState st
            failure
    
string :: String -> Parser String
string [] = return ""
string str@(x:xs) = char x >> (string xs) >> (return str)
    
char :: Char -> Parser ()
char c = cond (==c) single >> return ()

ws :: Parser ()
ws = (many $ char ' ' <|>  char '\t' <|> char '\n') >> return ()