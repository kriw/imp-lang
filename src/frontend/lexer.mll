{
open Parser
open Lexing
exception Error of string
}

let space = [' ' '\t']
let newline = ['\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let alnum = digit | alpha

rule token = parse
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMICOLON }
| '+' { OP_ADD }
| '-' { OP_SUB }
| '*' { OP_MUL }
| '/' { OP_DIV }
| '<' { OP_LT }
| '~' { OP_NOT }
| "<=" { OP_LTEQ }
| "==" { OP_EQ }
| "<>" { OP_NEQ }
| "&&" { OP_AND }
| "||" { OP_OR }
| "else" { ELSE }
| "if" { IF }
| "while" { WHILE }
| "var" { DEC_VAR }
| "true" { TRUE }
| "false" { FALSE }
| (lower alnum*) as po { VAR(po) }
| newline+ { token lexbuf }
| space+ { token lexbuf }
| (['1'-'9'] digit* | '0') as num { DIGITS(num) }
| '=' { EQ }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

