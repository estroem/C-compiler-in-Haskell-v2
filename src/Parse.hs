module Parse ( parse ) where

import Data.Char
import Data.List
import Debug.Trace

import Tokenize
import Ast
import Type
import Op

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

runP :: Parser a -> [String] -> Maybe ([String], a)
runP (P p) inp = p inp

failure :: Parser a
failure = P $ \ _ -> Nothing

getState :: Parser [String]
getState = P $ \ inp -> Just (inp, inp)

setState :: [String] -> Parser ()
setState s = P $ \ inp -> Just (s, ())

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (P a) (P b) = P $ \ inp -> case a inp of
    Just r -> Just r
    Nothing -> b inp

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
    case t of
        (FuncType _ _) -> return ()
        _ -> failure
    return $ FunDecl t n

func :: Parser Symb
func = do
    (FunDecl t n) <- funcDecl
    b <- block
    return $ Func t n [b]

block :: Parser Stmt
block = Block <$> (braces $ many stmt)

stmt :: Parser Stmt
stmt = block <|> if' <|> while <|> for <|> locVar <|> break' <|> continue <|> goto <|> label <|> return' <|> (Expr <$> expr <* semi)

locVar :: Parser Stmt
locVar = (gloVar <* semi >>= \ (VarDecl t n _) -> return $ LocVar t n False Nothing)
    <|> (initVar <* semi >>= \ (Init t n e) -> return $ LocVar t n False $ Just e)
    
if' :: Parser Stmt
if' = do
    string "if"
    c <- parens expr
    s1 <- stmt
    s2 <- (string "else" >> stmt) <|> return Nop
    return $ If c s1 s2

while :: Parser Stmt
while = (string "while") >> While <$> (parens expr) <*> stmt

for :: Parser Stmt
for = do
    string "for"
    char '('
    pre <- (stmt <|> (return Nop <* semi))
    cond <- (expr <|> return (Number 1)) <* semi
    post <- (expr <|> return (Number 0))
    char ')'
    body <- stmt
    return $ For pre cond post body

break' :: Parser Stmt
break' = string "break" >> semi >> return Break

continue :: Parser Stmt
continue = string "continue" >> semi >> return Continue

goto :: Parser Stmt
goto = string "goto" >> Goto <$> identifier <* semi

label :: Parser Stmt
label = GotoLabel <$> identifier <* char ':'

return' :: Parser Stmt
return' = string "return" >> (Return <$> (Just <$> expr) <|> return Nothing) <* semi

expr :: Parser Expr
expr = binAppR ["=", "+=", "-=", "*=", "/="] ternary

ternary :: Parser Expr
ternary = do
    cond <- disjuction
    (do
        char '?'
        ex1 <- disjuction
        char ':'
        ex2 <- ternary
        return $ Ternary cond ex1 ex2
     ) <|> return cond

disjuction :: Parser Expr
disjuction = binAppL ["||"] conjunction

conjunction :: Parser Expr
conjunction = binAppL ["&&"] bitOr

bitOr :: Parser Expr
bitOr = binAppL ["|"] bitXor

bitXor :: Parser Expr
bitXor = binAppL ["^"] bitAnd

bitAnd :: Parser Expr
bitAnd = binAppL ["&"] equivalence

equivalence :: Parser Expr
equivalence = binAppL ["==", "!="] relation

relation :: Parser Expr
relation = binAppL ["<", ">", "<=", ">="] shift

shift :: Parser Expr
shift = binAppL ["<<", ">>"] summation

summation :: Parser Expr
summation = binAppL ["+", "-"] term

term :: Parser Expr
term = binAppL ["*", "/"] unaryL

unaryL :: Parser Expr
unaryL = unAppL ["&", "!", "*", "++", "--"] unaryR

unaryR :: Parser Expr
unaryR = unAppR ["++", "--"] $ number <|> float <|> name <|> literal <|> (parens expr)

binAppL :: [String] -> Parser Expr -> Parser Expr
binAppL s p = p >>= (binAppL' s p) where
    binAppL' s p prev = (do
        op <- oneOf (map string s)
        e <- p
        binAppL' s p $ App op [prev, e]
     ) <|> return prev

binAppR :: [String] -> Parser Expr -> Parser Expr
binAppR s p = do
    e1 <- p
    (do
        op <- oneOf (map string s)
        e2 <- binAppR s p
        return $ App op [e1, e2]
     ) <|> return e1

unAppL :: [String] -> Parser Expr -> Parser Expr
unAppL s p = (do
        op <- preFix <$> oneOf (map string s)
        e <- unAppL s p
        return $ App op [e]
    ) <|> p

unAppR :: [String] -> Parser Expr -> Parser Expr
unAppR s p = p >>= (unAppR' s p) where
    unAppR' s p prev = (do
        op <- postFix <$> oneOf (map string s)
        unAppR' s p $ App op [prev]
     ) <|> call prev <|> arrayDeref prev <|> return prev
    call e = Call e <$> parens (sep (char ',') expr) >>= unAppR' s p
    arrayDeref e = ArrayDeref e <$> (brackets expr) >>= unAppR' s p

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

brackets :: Parser a -> Parser a
brackets p = (char '[') >> p <* (char ']')

literal :: Parser Expr
literal = do
    s <- cond (\ t -> head t == '"') single
    return $ Literal $ tail s
    
name :: Parser Expr
name = Name <$> identifier

number :: Parser Expr
number = Number <$> int

float :: Parser Expr
float = Float <$> floatnum
    
identifier :: Parser String
identifier = cond (all isAlphaNum) single

decl :: Parser (Type, String)
decl = do
    p <- (PrimType <$> cond isPrimitive single) <|> (string "void" >> return VoidType)
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
funcType = (FuncType EmptyType) <$> parens (sep (char ',') decl)

arrType :: Parser Type
arrType = (char '[') >> ((ArrayType EmptyType) <$> fromInteger <$> int) <* (char ']')

createType :: Int -> [Type] -> Type
createType 0 [] = EmptyType
createType p [] = PtrType $ createType (p-1) []
createType p ((ArrayType _ n):xs) = ArrayType (createType p xs) n
createType p ((FuncType _ a):xs) = FuncType (createType p xs) a

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
int = read <$> cond isInt single

isInt :: String -> Bool
isInt (x:xs) = (x == '-' || isDigit x) && (all isDigit xs)

floatnum :: Parser Float
floatnum = read <$> cond isFloat single

isFloat :: String -> Bool
isFloat (x:xs) = (x == '-' || isDigit x) && all (\ t -> isDigit t || t == '.') xs

string :: String -> Parser String
string s = cond (==s) single
    
char :: Char -> Parser Char
char c = cond (==[c]) single >>= return . head