## Building a C Compiler

```bash
Preprocessor -> Lexing -> Parsing -> Type Checking -> Intermediate Representation -> Optimization -> Code Generation
```

For this project a tiny fraction of C is used. The grammar of Tiny-C in EBNF is:
```bash
<program>    ::= <statement>
<statement>  ::= "if"    <paren_expr> <statement> |
                 "if"    <paren_expr> <statement> "else" <statement> |
                 "while" <paren_expr> <statement> |
                 "do"    <statement> "while" <paren_expr> ";" |
                 "{" {   <statement> } "}" |
                 <expr> ";" |
                 ";"
<paren_expr> ::= "(" <expr> ")"
<expr>       ::= <test> | <id>  "=" <expr>
<test>       ::= <sum>  | <sum> "<" <sum>
<sum>        ::= <term> | <sum> "+" <term> | <sum> "-" <term>
<term>       ::= <id>   | <int> | <paren_expr>
<id>         ::= "a" | "b" | "c" | "d" | ... | "z"
<int>        ::= <an_unsigned_decimal_integer>
```

(1) **Lexing**

The lexer turns source code into a stream of tokens. 

```bash
"a=b=c=2<3;\n" 
    ->  [(ID,"a"),(EQUAL,"="),(ID,"b"),(EQUAL,"="),(ID,"c"),(EQUAL,"="),(INT,"2"),(LESS,"<"),(INT,"3"),(SEMI,";"),(EOL,"\n")]
```

(2) **Parsing**

The parser takes up the tokens and outputs an abstract syntax tree (AST).

(3) **Code Generation**

### Run

Haskell compiler: [GHC](https://www.haskell.org/ghc/)
```bash
./run.sh
```

## Additional Information

- [Tinyc](http://www.iro.umontreal.ca/~felipe/IFT2030-Automne2002/Complements/tinyc.c): Tiny-C is a considerably stripped down version of C and it is meant as a pedagogical tool for learning about compilers.
