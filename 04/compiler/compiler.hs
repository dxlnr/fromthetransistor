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
        deriving (Show, Enum, Bounded)

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

l :: [Char] -> [Toks] 
l [] = []
l (c:cs)
    | (isDigit c)  = 
        let (_, after) = iWhileDigit (c:cs)
        in INT : l after
    | (c == '{')   = LBRA  : l cs
    | (c == '}')   = RBRA  : l cs
    | (c == '(')   = LPAR  : l cs
    | (c == ')')   = RPAR  : l cs
    | (c == ';')   = SEMI  : l cs
    | (c == '+')   = PLUS  : l cs
    | (c == '-')   = MINUS : l cs
    | (c == '=')   = EQUAL : l cs
    | (c == '<')   = LESS  : l cs
    | (isLetter c) = 
        let (w, after) = iWhileAlpha (c:cs)
        in mToks (isKword w) : l after
    | (c == '\n')  = EOL   : l cs
    | otherwise    = l cs 

---------------------------------------------------------
-- Parser

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
    print (l prog)
