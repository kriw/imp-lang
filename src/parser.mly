%{
open Syntax
}%

%token LPAREN RPALEN LBRACE RBRACE
%token EOF

%start parse
%type <term> parse
%%

statement:  | VAR ident = expr
            | ident = expr
            | IF LPAREN expr RPAREN 
                LBRACE statement RBRACE
                ELSE 
                LBRACE statement RBRACE
            | WHILE LPAREN expr RPAREN
                LBRACE statement RBRACE
expr:       | const
            | ident
            | expr OP expr
ident:      | VAR
const:      |
operator:   | OP { $1 }
