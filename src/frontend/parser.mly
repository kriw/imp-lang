%{
open Syntax
%}

%token IF ELSE DEC_VAR WHILE
%token LPAREN RPAREN LBRACE RBRACE
%token SEMICOLON EOF
%token <string> DIGITS VAR
%token OP_ADD OP_SUB OP_MUL OP_DIV EQ OP_LT TRUE FALSE
%token OP_LTEQ OP_EQ OP_NEQ OP_AND OP_OR OP_NOT

%start parse
%type <Syntax.statement> parse
%%

parse:      | statement EOF { $1 }
statement:  | DEC_VAR ident EQ expr { Define($2, $4) }
            | ident EQ expr { Assign($1, $3) }
            | IF LPAREN expr RPAREN
                LBRACE statement RBRACE
                { If($3, $6, None) }
            | IF LPAREN expr RPAREN 
                LBRACE statement RBRACE
                ELSE 
                LBRACE statement RBRACE
                { If($3, $6, Some $10) }
            | WHILE LPAREN expr RPAREN
                LBRACE statement RBRACE
                { While($3, $6) }
            | statement SEMICOLON statement
                { Seq($1, $3) }
            | (* empty *) { Emp }
expr:       | const { Const $1 }
            | ident { Ident $1 }
            | expr operator expr { Exprs($1, $2, $3) }
ident:      | VAR { $1 }
const:      | DIGITS { Int (int_of_string $1) }
            | boolean { Bool $1 }
boolean:    | TRUE { true }
            | FALSE { false }
operator:   | OP_ADD { Add }
            | OP_SUB { Sub }
            | OP_MUL { Mul }
            | OP_DIV { Div }
            | OP_LT { Lt }
            | OP_LTEQ { LtEq }
            | OP_AND { And }
            | OP_OR { Or }
            | OP_EQ { Eq }
            | OP_NEQ { Neq }
            | OP_NOT { Not }
