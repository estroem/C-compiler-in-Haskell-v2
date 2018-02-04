module Parse ( parse ) where

import Data.Char
import Data.List

import Tokenize
import Ast
import Type

data Parser a = P ([String] -> Maybe ([String], a))

parse :: [String] -> File
parse = maybe (error "") snd . p
    where (P p) = file

instance Functor Parser where
    fmap f (P p) = P $ \ inp -> case p inp of
        Just (inp', x) -> Just (inp', f x)
        Nothing -> Nothing

instance Applicative Parser where
    pure x = P $ \ inp -> Just (inp, x)
    (<*>) (P a) (P b) = P $ \ inp -> case a inp of
        Just (inp', f) -> case b inp' of
            Just (inp'', x) -> Just (inp'', f x)
            Nothing -> Nothing
        Nothing -> Nothing

instance Monad Parser where
    return x = P $ \ inp -> Just (inp, x)
    (>>=) (P a) f = P $ \ inp -> case a inp of
        Just (inp', x) -> let (P b) = (f x) in b inp'
        Nothing -> Nothing

failure :: Parser a
failure = P $ \ _ -> Nothing

getState :: Parser [String]
getState = P $ \ inp -> Just (inp, inp)

setState :: [String] -> Parser ()
setState s = P $ \ inp -> Just (s, ())

--(<*) :: Parser a -> Parser b -> Parser a
--(<*) a b = a >>= \ r -> b >> return r

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (P a) (P b) = P $ \ inp -> case a inp of
    Just r -> Just r
    Nothing -> b inp

stop :: Parser a -> Parser ()
stop (P p) = P $ \ inp -> case p inp of
        Just _ -> Nothing
        Nothing -> Just (inp, ())

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
file = File <$> many symb

symb :: Parser Symb
symb = func <|> ((funcDecl <|> initVar <|> gloVar) <* semi)

gloVar :: Parser Symb
gloVar = do
    (t, n) <- decl
    return $ VarDecl t n False

initVar :: Parser Symb
initVar = do
    (VarDecl t n _) <- gloVar
    char '='
    e <- expr
    return $ Init t n e

funcDecl :: Parser Symb
funcDecl = do
    (t, n) <- decl
    return $ FunDecl t n

func :: Parser Symb
func = do
    (FunDecl t n) <- funcDecl
    b <- block
    return $ Func t n [b]

block :: Parser Stmt
block = Block <$> (braces $ many stmt)

stmt :: Parser Stmt
stmt = block <|> if' <|> while <|> locVar <|> (return' <* semi) <|> (Expr <$> expr <* semi)

locVar :: Parser Stmt
locVar = (gloVar <* semi >>= \ (VarDecl t n _) -> return $ LocVar t n False Nothing)
    <|> (initVar <* semi >>= \ (Init t n e) -> return $ LocVar t n False $ Just e)
    
if' :: Parser Stmt
if' = do
    string "if"
    c <- parens expr
    s1 <- stmt
    s2 <- (string "else" >> stmt) <|> return (Block [])
    return $ If c s1 s2

while :: Parser Stmt
while = (string "while") >> While <$> (parens expr) <*> stmt

return' :: Parser Stmt
return' = string "return" >> Return <$> (Just <$> expr) <|> return Nothing

expr :: Parser Expr
expr = binAppR ["="] disjuction

disjuction :: Parser Expr
disjuction = binAppR ["||"] conjunction

conjunction :: Parser Expr
conjunction = binAppR ["&&"] relation

relation :: Parser Expr
relation = binAppR ["==", "!=", "<", ">", "<=", ">="] summation

summation :: Parser Expr
summation = binAppR ["+", "-"] term

term :: Parser Expr
term = binAppR ["*", "/"] unary

unary :: Parser Expr
unary = unApp ["$", "&", "!"] (call <|> number <|> name <|> literal <|> (parens expr))

binAppR :: [String] -> Parser Expr -> Parser Expr
binAppR s p = do
    e1 <- p
    (do
        op <- oneOf (map string s)
        e2 <- binAppR s p
        return $ App op [e1, e2]
     ) <|> return e1

unApp :: [String] -> Parser Expr -> Parser Expr
unApp s p = (do
        op <- oneOf (map string s)
        e <- p
        return $ App op [e]
    ) <|> p
     
semi :: Parser ()
semi = char ';' >> return ()

oneOf :: [Parser a] -> Parser a
oneOf [] = failure
oneOf (x:xs) = x <|> oneOf xs

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
    s <- cond (\ t -> head t == '"') single
    return $ Literal $ tail s

call :: Parser Expr
call = do
    e <- name <|> parens expr
    a <- parens $ sep (char ',') expr
    return $ Call e a

name :: Parser Expr
name = Name <$> identifier

number :: Parser Expr
number = Number <$> int
    
identifier :: Parser String
identifier = cond (all isAlphaNum) single

decl :: Parser (Type, String)
decl = do
    p <- PrimType <$> cond isPrimitive single
    (t, n) <- innerDecl
    return (addType t p, n)

innerDecl :: Parser (Type, String)
innerDecl = do
    x <- length <$> (many $ char '*')
    (t, n) <- (identifier >>= \ n -> return (EmptyType, n))
        <|> parens innerDecl
        <|> return (EmptyType, "")
    a <- many $ oneOf [funcType, arrType]
    return (addType t (createType x a), n)

funcType :: Parser Type
funcType = do
    char '('
    a <- sep (char ',') decl
    char ')'
    return $ FuncType EmptyType a

arrType :: Parser Type
arrType = (char '[') >> ((ArrayType EmptyType) <$> fromInteger <$> int) <* (char ']')

createType :: Int -> [Type] -> Type
createType 0 [] = EmptyType
createType p [] = PtrType $ createType (p-1) []
createType p ((ArrayType _ n):xs) = ArrayType (createType p xs) n
createType p ((FuncType _ a):xs) = FuncType (createType p xs) a

peek :: Parser a -> Parser a
peek p = do
    st <- getState
    r <- p
    setState st
    return r

single :: Parser String
single = P $ \ inp ->
    if inp /= []
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
    
int :: Parser Integer
int = read <$> cond (all isDigit) single

string :: String -> Parser String
string s = cond (==s) single
    
char :: Char -> Parser Char
char c = cond (==[c]) single >>= return . head