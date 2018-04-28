{
open Parser
open Lexing
open exception Error of string
}

let ops = ['+', '-', '*', '%']
let space = [' ', '\t']
let newline = ['\n', '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let alnum = digit | alpha;;

rule token = parse
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| "else" { ELSE }
| "if" { IF }
| "while" { WHILE }
| lower alnum* { VAR }
| ops { OP }
| newline+ { token lexbuf }
| space+ { token lexbuf }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)))

