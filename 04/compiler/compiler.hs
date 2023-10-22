import Debug.Trace
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
         | LESSTHAN
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

-- data Node = Node { kind :: Sym, o1 :: Maybe Node, o2 :: Maybe Node } deriving (Show)
data AST a = Empty
           | Node a [AST a] String
        deriving (Show, Eq)

-- insert :: a -> AST a -> AST a
-- insert x Empty = Node x []
-- insert x (Node y childs) = Node y (childs ++ [Node x []])

insert :: AST a -> AST a -> AST a
insert x  Empty = x 
insert x (Node y children v) = Node y (x: children) v

-- <term> ::= <id> | <int> | <paren_expr>
pterm :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
pterm [] = (Empty, [])
pterm t@((toks, str):ts) 
    | trace ("pterm " ++ show t) False = undefined 
    | (toks == ID)  = (Node VAR [] str, ts) 
    | (toks == INT) = (Node CST [] str, ts) 
    | otherwise     = (Node SET [] str, (ts))

-- <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
psum :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
psum [] = (Empty, [])
psum ((toks, str):ts) 
    | otherwise = pterm ((toks, str):ts) 

-- <test> ::= <sum> | <sum> "<" <sum>
ptest :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
ptest [] = (Empty, [])
ptest ((toks, str):ts) 
     -- | (toks == LESS) = (Node LESSTHAN [] "", ts) 
     | otherwise = psum ((toks, str):ts) 

-- <expr> ::= <test> | <id> "=" <expr>
expr :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
expr [] = (Empty, [])
expr t@((toks, str):ts)
    | trace ("expr " ++ show t) False = undefined 
    -- | (toks /= ID) = 
    --     let (n, after) = ptest ((toks, str):ts)
    --     in  (n, after)
    | otherwise =  
        let na@(n, after) = ptest ((toks, str):ts)
            ast = insert n (fst (expr after))
        in trace ("expr na: " ++ show na) (ast, snd (expr after)) 
--
-- expr :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
-- expr [] = (Empty, [])
-- expr ((toks, str):ts) 
--     | (toks /= ID) = ptest ((toks, str):ts)
--     | otherwise = 
--     -- let (n, after) = ptest ((toks, str):ts)
--         -- (nextAST, remainingTokens) = expr after
--     let (currentAST, afterCurrent) = ptest ((toks, str):ts)
--         (nextAST1, afterNext1) = expr afterCurrent
--         (nextAST2, remainingTokens) = expr afterNext1
--     in if (toks == ID) 
--         then let combinedAST1 = insert currentAST nextAST1
--                  combinedAST2 = insert combinedAST1 nextAST2
--              in (combinedAST2, remainingTokens)
--         -- then (insert n nextAST, remainingTokens)
--         -- else (n, ts)
--         else (Node EMPTY [] "", ts)

-- expr :: [(Toks, String)] -> (AST Sym, [(Toks, String)])
-- expr [] = (Empty, [])
-- expr ((toks, str):ts)
--     | toks /= ID = 
--         let (parsedNode, remaining) = ptest ((toks, str):ts)
--         in (parsedNode, remaining)
--     | toks == ID = 
--         let (currentAST, afterCurrent) = ptest ((toks, str):ts)
--             (nextAST1, afterNext1) = expr afterCurrent
--             (nextAST2, remainingTokens) = expr afterNext1
--             combinedAST1 = insert currentAST nextAST1
--             combinedAST2 = insert combinedAST1 nextAST2
--         in (combinedAST2, remainingTokens)


par :: [(Toks, String)] -> AST Sym
par [] = Node PROG [] ""
par ((toks, str):ts)
    | (toks == SIF)    = par ts
    | (toks == SDO)    = par ts
    | (toks == SWHILE) = par ts
    | (toks == LBRA)   = par ts
    | (toks == SEMI)   = par ts
    | otherwise = 
        let (n, after) = (expr ((toks, str):ts))
        in insert n (par after)
        -- if (toks /= SEMI) then error "syntax error"

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
