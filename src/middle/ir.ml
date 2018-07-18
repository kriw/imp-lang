exception TypeError;;
exception InvalidOperand;;
exception TODO;;

type opcode = 
  | Assign
  | JmpIf
  | Jmp
  | Add
  | Sub
  | Mul
  | Mod
  | Div
  | Eq
  | Neq
  | And
  | Or
  | Lt
  | LtEq

type node =
    | EntryNode
    | ActionNode of opcode
    | ValueNode of string
type edge =
    | NextEdge of node * node
    | ValueEdge of node * node
    | JmpEdge of node * node

let edges = ref []
let nodes = ref [EntryNode]

let add_edge e = edges := e :: !edges
let add_node n = nodes := n :: !nodes

module Vars = Map.Make(String);;

let vars = ref Vars.empty

let emit_operator op x y =
    match op with
    | Syntax.Add -> raise TODO
    | Syntax.Sub -> raise TODO
    | Syntax.Mul -> raise TODO
    | Syntax.Mod -> raise TODO
    | Syntax.Div -> raise TODO
    | Syntax.Eq  -> raise TODO
    | Syntax.Neq -> raise TODO
    | Syntax.Lt  -> raise TODO
    | Syntax.LtEq -> raise TODO
    | Syntax.And -> raise TODO
    | Syntax.Or  -> raise TODO

let emit_assign i e =
    match e with
    | Syntax.Const _ -> raise TODO
    | Syntax.Ident _ -> raise TODO
    | Syntax.Exprs _ -> raise TODO

let rec emit_statement s =
    match s with
    | Syntax.Define _ -> raise TODO
    | Syntax.Assign (i, e) -> emit_assign i e
    | Syntax.If (cond, if_then, Some if_else) -> raise TODO
    | Syntax.If (cond, if_then, None) -> raise TODO
    | Syntax.While _ -> raise TODO
    | Syntax.Seq (s1, s2) ->
        let _ = emit_statement s1 in
        let _ = emit_statement s2 in
        raise TODO
    | Syntax.Emp -> ""
