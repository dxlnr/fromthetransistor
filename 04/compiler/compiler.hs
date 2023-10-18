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
mToks Nothing  = NONE

iWhileAlpha :: [Char] -> [Char] 
iWhileAlpha [] = []
iWhileAlpha (c:cs)
    | isAlpha c = c : iWhileAlpha cs
    | otherwise = []

-- getKeyword :: 
-- getKeyword [c]
--     | (isLetter c) = getKeyword c
--     | otherwise = ID

l :: [Char] -> [Toks] 
l [] = []
l (c:cs)
    | (isDigit c)  = INT   : l cs
    | (c == '{')   = LBRA  : l cs
    | (c == '}')   = RBRA  : l cs
    | (c == '(')   = LPAR  : l cs
    | (c == ')')   = RPAR  : l cs
    | (c == ';')   = SEMI  : l cs
    | (c == '+')   = PLUS  : l cs
    | (c == '-')   = MINUS : l cs
    | (c == '=')   = EQUAL : l cs
    | (c == '<')   = LESS  : l cs
    | (isLetter c) = mToks (isKword (iWhileAlpha (c:cs))) : l cs
    | (c == '\n')  = EOL   : l cs
    | otherwise    = NONE  : l cs 

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
