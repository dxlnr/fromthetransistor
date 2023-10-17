import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Data.Typeable (typeOf)

-- Tokens : Keywords, Operators, Strings, Constants, Special Characters, Identifiers.
data Token = Key | Ops | Str | Const | SChar | Ident

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
        deriving (Show, Enum, Bounded)

instance Show Token where 
    show Key = "Keyword"
    show Ops = "Operator"
    show Str = "String"
    show Const = "Constant"
    show SChar = "Special Character"
    show Ident = "Identifier"

-------------------------------------------------------------------------------
-- Lexer
lexer :: Char -> Token
lexer c 
    | (c == ','|| c == '.'|| c == ';'|| c ==':'|| c =='?'|| c =='*') = SChar
    | otherwise = Str 

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
