import Debug.Trace
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Data.Typeable (typeOf)
import Data.Char (isDigit, isLetter, toUpper, isAlpha)

---------------------------------------------------------
-- Lexer
data Tokid = SDO 
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

data Toks = Toks { tok :: Tokid, v :: String } deriving (Show, Eq)

kwords :: [(Maybe String, Tokid)]
kwords =  [
    (Just "do", SDO), 
    (Just "else", SELSE),
    (Just "if", SIF),
    (Just "while", SWHILE), 
    (Nothing, ID)
    ]

isKword :: String -> Maybe Tokid
isKword w = 
    lookup (Just w) kwords

mToks :: Maybe Tokid -> Tokid
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

lx :: [Char] -> [Toks] 
lx [] = []
lx (c:cs)
    | (isDigit c)  = 
        let (d, after) = iWhileDigit (c:cs)
        in Toks INT d  : lx after
    | (c == '{')   = Toks LBRA  [c] : lx cs
    | (c == '}')   = Toks RBRA  [c] : lx cs
    | (c == '(')   = Toks LPAR  [c] : lx cs
    | (c == ')')   = Toks RPAR  [c] : lx cs
    | (c == ';')   = Toks SEMI  [c] : lx cs
    | (c == '+')   = Toks PLUS  [c] : lx cs
    | (c == '-')   = Toks MINUS [c] : lx cs
    | (c == '=')   = Toks EQUAL [c] : lx cs
    | (c == '<')   = Toks LESS  [c] : lx cs
    | (isLetter c) = 
        let (w, after) = iWhileAlpha (c:cs)
        in Toks (mToks (isKword w)) w : lx after
    | (c == '\n')  = Toks EOL [c]   : lx cs
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
        deriving (Show, Enum, Bounded, Eq)

data Node = Node { 
        kind :: Sym, 
        o1   :: Maybe Node, 
        o2   :: Maybe Node, 
        str  :: String 
    } deriving (Show)

next_tok :: [Toks] -> (Toks, [Toks])
next_tok [] = error "No more tokens."
next_tok (t:ts) = (t, ts)

-- data AST a = Empty
--            | Node a [AST a] String
--         deriving (Show, Eq)

---- insert :: a -> AST a -> AST a
---- insert x Empty = Node x []
---- insert x (Node y childs) = Node y (childs ++ [Node x []])

-- insert :: AST a -> AST a -> AST a
-- insert x  Empty = x 
-- insert x (Node y children v) = Node y (x: children) v

-- <term> ::= <id> | <int> | <paren_expr>
pterm :: [Toks] -> (Node, [Toks])
pterm (t:ts) 
    | (tok t == ID)  = (Node VAR Nothing Nothing (v t), ts) 
    | (tok t == INT) = (Node CST Nothing Nothing (v t), ts) 
    | otherwise = expr ts

-- <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
psum :: [Toks] -> (Node, [Toks])
psum (t:ts) 
    | otherwise = pterm (t:ts) 

-- <test> ::= <sum> | <sum> "<" <sum>
ptest :: [Toks] -> (Node, [Toks])
ptest (t:ts) =
    let (n, after) = psum (t:ts) 
        (tt, tafter) = next_tok after
    in if tok tt == LESS
       then 
           let (n2, after2) = psum tafter
           in (Node LESSTHAN (Just n) (Just n2) "<", after2)
       else (n, after)

-- <expr> ::= <test> | <id> "=" <expr>
expr :: [Toks] -> (Node, [Toks])
expr (t:ts)
    | tok t /= ID = ptest (t:ts) 
    | otherwise =
        let (n, after) = ptest (t:ts) in
        if kind n == VAR && tok (head after) == EQUAL
            then let (rn, more) = expr (after) in
                 (Node SET (Just n) (Just rn) "=", (t:more))
            else (n, after)

-- <paren_expr> ::= "(" <expr> ")"
exprPar :: [Toks] -> (Node, [Toks])
exprPar dtt@(t:ts)
    | trace ("Debugging value of t: " ++ show dtt) False = undefined 
    | tok t == LPAR =
        let na@(tt, tafter) = next_tok (t:ts)
            (nn, nafter) = expr (t:ts)
            dM = trace ("exprPar na: " ++ show na) ()
        in dM `seq` 
        if tok (head nafter) == RPAR
            then (nn, nafter)
            else error "syntax error"
    | otherwise = error "syntax error"

stmt :: [Toks] -> (Node, [Toks]) 
stmt (t:ts)
     | (tok t == SIF)    = stmt ts
     | (tok t == SDO)    = stmt ts
     | (tok t == SWHILE) = 
        (Node WHILE (Just (fst (exprPar (t:ts)))) (Just (fst (stmt ts))) "", ts)
        
        
     | (tok t == LBRA)   = stmt ts
     | (tok t == SEMI)   = stmt ts
     | otherwise = 
        let (n, after) = expr (t:ts)
        in (Node EXPR (Just n) Nothing "", ts)

par :: [Toks] -> Node 
par [] = error "No source code provided." 
par (t:ts)
    | (tok t == EOL) = error "syntax error"
    | otherwise =
        let (n, after) = stmt (t:ts)
        in Node PROG (Just n) Nothing ""

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
