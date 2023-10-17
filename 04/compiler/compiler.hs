import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Data.Typeable (typeOf)
import Data.Char (isDigit, isLetter)

-------------------------------------------------------------------------------
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
          | EOF
          | NONE
        deriving (Show, Enum, Bounded)

lexer :: Char -> Toks
lexer c 
    | (isDigit c)  = INT
    | (c == '{')   = LBRA
    | (c == '}')   = RBRA
    | (c == '(')   = LPAR
    | (c == ')')   = RPAR
    | (c == ';')   = SEMI
    | (c == '+')   = PLUS
    | (c == '-')   = MINUS
    | (c == '=')   = EQUAL
    | (c == '<')   = LESS
    | (isLetter c) = ID
    | (c == '\n')  = EOF
    | otherwise    = NONE

-- Iterate over every single character.
l c = [ lexer x | x <- c ]

-------------------------------------------------------------------------------
-- Parser

-------------------------------------------------------------------------------
-- Code generator


-------------------------------------------------------------------------------
-- Virtual machine


-------------------------------------------------------------------------------
-- Main program
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    prog <- hGetContents file
    print prog
    print (typeOf prog)
    print (l prog)
