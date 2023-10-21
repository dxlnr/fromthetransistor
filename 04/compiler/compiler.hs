import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Data.Typeable (typeOf)
import Data.Char (isDigit, isLetter, toUpper, isAlpha)

---------------------------------------------------------
-- Lexer
data Toks = SDO 
          | SELSE
          | SIF
          | SWHILE
          | LBRA
          | RBRA
          | LPAR
          | RPAR
          | PLUS
          | MINUS
          | LESS
          | SEMI
          | EQUAL
          | INT
          | ID
          | EOL
          | NONE
        deriving (Show, Enum, Bounded, Eq)

kwords :: [(Maybe String, Toks)]
kwords =  [
    (Just "do", SDO), 
    (Just "else", SELSE),
    (Just "if", SIF),
    (Just "while", SWHILE), 
    (Nothing, ID)
    ]

isKword :: String -> Maybe Toks
isKword w = 
    lookup (Just w) kwords

mToks :: Maybe Toks -> Toks
mToks (Just x) = x
mToks Nothing  = ID 

iWhileAlpha :: String -> (String, String)
iWhileAlpha [] = ([], [])
iWhileAlpha (x:xs)
    | isAlpha x =
        let (w, after) = iWhileAlpha xs
        in (x:w, after)
    | otherwise = ([], x:xs)

iWhileDigit:: String -> (String, String)
iWhileDigit [] = ([], [])
iWhileDigit (x:xs)
    | isDigit x =
        let (w, after) = iWhileDigit xs
        in (x:w, after)
    | otherwise = ([], x:xs)

lx :: [Char] -> [(Toks, String)] 
lx [] = []
lx (c:cs)
    | (isDigit c)  = 
        let (d, after) = iWhileDigit (c:cs)
        in (INT, d)  : lx after
    | (c == '{')   = (LBRA,  [c]) : lx cs
    | (c == '}')   = (RBRA,  [c]) : lx cs
    | (c == '(')   = (LPAR,  [c]) : lx cs
    | (c == ')')   = (RPAR,  [c]) : lx cs
    | (c == ';')   = (SEMI,  [c]) : lx cs
    | (c == '+')   = (PLUS,  [c]) : lx cs
    | (c == '-')   = (MINUS, [c]) : lx cs
    | (c == '=')   = (EQUAL, [c]) : lx cs
    | (c == '<')   = (LESS,  [c]) : lx cs
    | (isLetter c) = 
        let (w, after) = iWhileAlpha (c:cs)
        in (mToks (isKword w), w) : lx after
    | (c == '\n')  = (EOL, [c])   : lx cs
    | otherwise    = lx cs 

---------------------------------------------------------
-- Parser
data Sym = VAR 
         | CST
         | ADD
         | SUB
         | LT
         | SET
         | IF1
         | IF2
         | WHILE
         | DO
         | EMPTY
         | SEQ
         | EXPR
         | PROG
        deriving (Show, Enum, Bounded)

data AST a = Empty
           | Node a [AST a]
        deriving (Show, Eq)

insert :: a -> AST a -> AST a
insert x Empty = Node x []
insert x (Node y childs) = Node y (childs ++ [Node x []])

par :: [(Toks, String)] -> AST (Toks, String)
par []     = Empty
par ((toks, str):ts)
    | (toks == SIF)    = par ts
    | (toks == SDO)    = par ts
    | (toks == SWHILE) = par ts
    | (toks == LBRA)   = par ts
    | otherwise = insert (toks, str) (par ts)

-- expr :: [(Toks, String)] -> AST
-- expr [] = Empty 
-- expr ((toks, str):ts)
--     | otherwise = expr ts
        
---------------------------------------------------------
-- Code generator

---------------------------------------------------------
-- Virtual machine

---------------------------------------------------------
-- Main program
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    prog <- hGetContents file
    print prog
    print (typeOf prog)

    let toks = lx prog
    let ast  = par toks

    print (toks)
    putStrLn ""
    print (ast)
