%{
open Syntax
%}

%token IF ELSE DEC_VAR WHILE
%token LPAREN RPAREN LBRACE RBRACE
%token SEMICOLON EOF
%token <string> DIGITS EQ VAR
%token <string> OP_ADD OP_SUB OP_MUL OP_DIV

%start parse
%type <Syntax.statement> parse
%%

parse:      | statement EOF { $1 }
statement:  | DEC_VAR ident EQ expr { Define($2, $4) }
            | ident EQ expr { Assign($1, $3) }
            | IF LPAREN expr RPAREN 
                LBRACE statement RBRACE
                ELSE 
                LBRACE statement RBRACE
                { If($3, $6, $10) }
            | WHILE LPAREN expr RPAREN
                LBRACE statement RBRACE
                { While($3, $6) }
            | statement SEMICOLON statement
                { Seq($1, $3) }
expr:       | digits { Ident $1 }
            | ident { Ident $1 }
            | expr operator expr { Exprs($1, $2, $3) }
ident:      | VAR { $1 }
digits:     | DIGITS { $1 }
operator:   | OP_ADD { Add $1 }
            | OP_SUB { Sub $1 }
            | OP_MUL { Mul $1 }
            | OP_DIV { Div $1 }
