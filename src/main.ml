open Syntax

let eval_bool_expr e =
    match e with
    | Const Bool True -> true
    | Const Bool False -> false
    (* TODO *)
    | _ -> false

(* TODO *)
let eval_const x = 1

(* TODO *)
let eval_ident x = 1

(* TODO *)
let eval_operator x y z = 1

let rec eval_expr e =
    match e with
    | Const c -> eval_const c
    | Ident i -> eval_ident i
    | Exprs (e1, op, e2) -> 
        let x = eval_expr e1 in
        let y = eval_expr e2 in
        eval_operator op x y

(* TODO *)
let eval_define i e = ()

(* TODO *)
let eval_assign i e = ()

let rec eval_statement s =
    match s with
    | Define (i, e) -> eval_define i e
    | Assign (i, e) -> eval_assign i e
    | If (cond, if_then, if_else) ->
            if eval_bool_expr cond then
                eval_statement if_then
            else
                eval_statement if_else
    | While (cond, s) ->
            if eval_bool_expr cond then
                let _ = eval_statement s in
                eval_statement (While (cond, s))
    | Seq (s1, s2) ->
            let _ = eval_statement s1 in
            eval_statement s2

let main () =
    let stdinbuf = Lexing.from_channel stdin in
    try
        let stmt = (Parser.parse Lexer.token stdinbuf) in
        let _ = Printf.printf "%s" (Syntax.show_statement stmt) in
        eval_statement stmt
    with
  | Lexer.Error msg ->
          Printf.eprintf "%s%!" msg
  | Parser.Error ->
          Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start stdinbuf)

let () = main ()
