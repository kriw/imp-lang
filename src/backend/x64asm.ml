open Syntax

exception TypeError;;
exception InvalidOperand;;
exception TODO;;

module Vars = Map.Make(String);;

let vars = ref Vars.empty

let emit_operator op x y =
    match op with
    | Add -> raise TODO
    | Sub -> raise TODO
    | Mul -> raise TODO
    | Mod -> raise TODO
    | Div -> raise TODO
    | Eq  -> raise TODO
    | Neq -> raise TODO
    | Lt  -> raise TODO
    | LtEq -> raise TODO
    | And -> raise TODO
    | Or  -> raise TODO

let emit_assign i e =
    match e with
    | Const _ -> raise TODO
    | Ident _ -> raise TODO
    | Exprs _ -> raise TODO

let rec emit_statement s =
    match s with
    | Define _ -> raise TODO
    | Assign _ -> raise TODO
    | If _ -> raise TODO
    | While _ -> raise TODO
    | Seq (s1, s2) ->
        let first = emit_statement s1 in
        let second = emit_statement s2 in
        first ^ second
    | Emp -> ""
        

