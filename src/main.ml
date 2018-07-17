open Syntax

exception TypeError;;
exception InvalidOperand;;

module Env = Map.Make(String);;

let env = ref Env.empty

let eval_ident x = Env.find x !env

let eval_operator op x y =
    let x_val = match x with
    | Exprs _ -> raise TypeError
    | Ident i -> eval_ident i
    | Const c -> c in
    let y_val = match y with
    | Exprs _ -> raise TypeError
    | Ident i -> eval_ident i
    | Const c -> c in
    match (op, x_val, y_val) with
    | (Add, Int a, Int b) -> Int (a + b)
    | (Sub, Int a, Int b) -> Int (a - b)
    | (Mul, Int a, Int b) -> Int (a * b)
    | (Mod, Int a, Int b) -> Int (a mod b)
    | (Div, Int a, Int b) -> Int (a / b)
    | (Eq, Int a, Int b) -> Bool (a = b)
    | (Neq, Int a, Int b) -> Bool (a <> b)
    | (Lt, Int a, Int b) -> Bool (a < b)
    | (LtEq, Int a, Int b) -> Bool (a <= b)
    | (And, Bool b1, Bool b2) -> Bool (b1 && b2)
    | (Or, Bool b1, Bool b2) -> Bool (b1 || b2)
    | _ -> raise InvalidOperand

let rec eval_exprs e =
    match e with
    | Exprs (e1, op, e2) -> 
        let x = eval_exprs e1 in
        let y = eval_exprs e2 in
        eval_operator op (Const x) (Const y)
    | Ident i -> Env.find i !env
    | Const c -> c

let eval_bool_expr e =
    let v = eval_exprs e in
    match v with
    | Bool b -> b
    | _ -> raise TypeError

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
    | If (cond, if_then, None) ->
            if eval_bool_expr cond then
                eval_statement if_then
    | If (cond, if_then, Some(if_else)) ->
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
    | Emp -> ()

let main () =
    let stdinbuf = Lexing.from_channel stdin in
    try
        let stmt = (Parser.parse Lexer.token stdinbuf) in
        let _ = Printf.printf "%s\n" (Syntax.show_statement stmt) in
        let _ = eval_statement stmt in
        let _ = Printf.printf "Env:\n" in
        Env.iter (fun k v -> Printf.printf "%s = %s\n" k (Syntax.show_const v)) !env
    with
  | Lexer.Error msg ->
          Printf.eprintf "%s%!" msg
  | Parser.Error ->
          Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start stdinbuf)

let () = main ()
