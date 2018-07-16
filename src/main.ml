open Syntax

module Env = Map.Make(String);;

let env = ref Env.empty

let eval_bool_expr e =
    match e with
    | Const Bool True -> true
    | Const Bool False -> false
    (* TODO raise exception *)
    | _ -> false

let eval_ident x = Env.find x !env

let is_bool_op op =
    match op with
    | And -> true
    | Or -> true
    | _ -> false

(* TODO *)
let eval_operator op x y =
    let _ = match (x, y) with
    (* TODO raise exception *)
    | (Exprs (_, _, _), _) -> ()
    (* TODO raise exception *)
    | (_, Exprs (_, _, _)) -> ()
    | _ -> () in
    if is_bool_op op then
        match (op, x, y) with
        | (Or, Const Bool b1, Const Bool b2) -> Int "1"
        (* TODO raise exception *)
        | _ -> Int "1"
    else
        match (op, x, y) with
        (*  TODO *)
        | (Add, _, _) -> Int "1"
        | (Sub, _, _) -> Int "1"
        | (Mul, _, _) -> Int "1"
        | (Mod, _, _) -> Int "1"
        | (Div, _, _) -> Int "1"
        | (Eq, _, _) -> Int "1"
        | (Neq, _, _) -> Int "1"
        | (Lt, _, _) -> Int "1"
        | (LtEq, _, _) -> Int "1"
        (* TODO raise exception *)
        | _ -> Int "1"

let rec eval_exprs e =
    match e with
    | Exprs (e1, op, e2) -> 
        let x = eval_exprs e1 in
        let y = eval_exprs e2 in
        eval_operator op (Const x) (Const y)
    | Ident i -> Env.find i !env
    | Const c -> c

let eval_define i e =
    match e with
    | Const c -> env := Env.add i c !env
    | Ident src ->
        let v = Env.find src !env in
        env := Env.add i v !env
    | Exprs _ ->
        let v = eval_exprs e in
        env := Env.add i v !env

let eval_assign i e =
    match e with
    | Const c -> env := Env.add i c !env
    | Ident src ->
        let v = Env.find src !env in
        env := Env.add i v !env
    | Exprs _ ->
        let v = eval_exprs e in
        env := Env.add i v !env

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
