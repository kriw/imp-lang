
## BNF

```
<toplevel> ::= <statement>*

<statement> ::= var <ident> = <expr>
              | <ident> = <expr>
              | if (<expr>) { <statement>* } else { <statement> }
              | while (<expr>) { <statement>* }

<ident> ::= [a-zA-Z][a-zA-Z0-9_]*

<expr> ::= <const> 
         | <ident>
         | <expr> <operator> <expr>

<operator> ::= + | - | * | / | % | == | != | and | or | < | <=;

<const> ::= <Bool> | <Int>

<Bool> ::= true | false

<Int> ::= [1-9][0-9]*
