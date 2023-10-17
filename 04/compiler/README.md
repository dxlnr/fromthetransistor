## Building a C Compiler

### Run

Haskell compiler: [GHC](https://www.haskell.org/ghc/)
```bash
ghc compiler.hs -o hc && ./hc
```

### Setup

The grammar of Tiny-C in EBNF is:
```bash
<program> ::= <statement>
<statement> ::= "if" <paren_expr> <statement> |
                "if" <paren_expr> <statement> "else" <statement> |
                "while" <paren_expr> <statement> |
                "do" <statement> "while" <paren_expr> ";" |
                "{" { <statement> } "}" |
                <expr> ";" |
                ";"
<paren_expr> ::= "(" <expr> ")"
<expr> ::= <test> | <id> "=" <expr>
<test> ::= <sum> | <sum> "<" <sum>
<sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term>
<term> ::= <id> | <int> | <paren_expr>
<id> ::= "a" | "b" | "c" | "d" | ... | "z"
<int> ::= <an_unsigned_decimal_integer>
```

## Additional Information

- [Tinyc](http://www.iro.umontreal.ca/~felipe/IFT2030-Automne2002/Complements/tinyc.c): Tiny-C is a considerably stripped down version of C and it is meant as a pedagogical tool for learning about compilers.
