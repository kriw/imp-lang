exception TypeError;;
exception InvalidOperand;;
exception TODO;;

let condition = "condition"

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
  | Nop
[@@deriving show]

let node_id = ref 0
let edge_id = ref 0

let new_node_id () =
    let ret = !node_id in
    let _ = node_id := ret + 1 in
    ret

let new_edge_id () =
    let ret = !edge_id in
    let _ = edge_id := ret + 1 in
    ret

type nodeId = int
[@@deriving show]

type edgeId = int
[@@deriving show]

type valueNode =
    | ConstInt of int
    | ConstBool of bool
    | Variable of string
[@@deriving show]

type node =
    | EntryNode
    | ActionNode of nodeId * opcode
    | ValueNode of nodeId * valueNode
[@@deriving show]

type edge =
    | NextEdge of edgeId * nodeId * nodeId
    | ValueEdge of edgeId * nodeId * nodeId
    | JmpEdge of edgeId * nodeId * nodeId
[@@deriving show]

let edges = ref []
let nodes = ref [EntryNode]

let add_edge e = edges := e :: !edges
let add_node n = nodes := n :: !nodes
let find_edge eid =
    let rec find_rec edges =
        match edges with
        | NextEdge (id, _, _) :: _ when id == eid -> Some (List.hd edges)
        | JmpEdge (id, _, _) :: _ when id == eid -> Some (List.hd edges)
        | ValueEdge (id, _, _) :: _ when id == eid -> Some (List.hd edges)
        | _ :: _ -> find_rec edges
        | _ -> None in
    find_rec !edges

let find_node nid =
    let rec find_rec nodes =
        match nodes with
        | ActionNode (id, _) :: _ when id == nid -> Some (List.hd nodes)
        | ValueNode (id, _) :: _ when id == nid -> Some (List.hd nodes)
        | _ :: ns -> find_rec ns
        | _ -> None in
    find_rec !nodes

let next_edge from_node to_node =
    let eid = new_edge_id () in
    let edge = NextEdge (eid, from_node, to_node) in
    let _ = add_edge edge in
    eid

let jmp_edge from_node to_node =
    let eid = new_edge_id () in
    let edge = JmpEdge (eid, from_node, to_node) in
    let _ = add_edge edge in
    eid

let new_var name =
    let nid = new_node_id () in
    let node = ValueNode (nid, Variable name) in
    let _ = add_node node in
    nid

let new_action action =
    let nid = new_node_id () in
    let node = ActionNode (nid, action) in
    let _ = add_node node in
    nid

(* Follow node of if statement *)
let nop_node () =
    let nid = new_node_id () in
    let node = ActionNode (nid, Nop) in
    let _ = add_node node in
    nid

let follow_node = nop_node

module Vars = Map.Make(String);;

let init_vars vars =
    let declare var_name = let _ = new_var var_name in () in
    List.iter declare vars

let rec gather_vars s =
    match s with
    | Syntax.Define (i, _) -> [i]
    | Syntax.If (_, if_then, Some if_else) ->
            let vars1 = gather_vars if_then in
            let vars2 = gather_vars if_else in
            List.append vars1 vars2
    | Syntax.If (_, if_then, None) -> gather_vars if_then
    | Syntax.While (_, st) -> gather_vars st
    | Syntax.Seq (s1, s2) ->
        let vars1 = gather_vars s1 in
        let vars2 = gather_vars s2 in
        List.append vars1 vars2
    | _ -> []

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
    (* | Syntax.Const _ -> raise TODO *)
    (* | Syntax.Ident _ -> raise TODO *)
    (* | Syntax.Exprs _ -> raise TODO *)
    | Syntax.Const _ -> nop_node ()
    | Syntax.Ident _ -> nop_node ()
    | Syntax.Exprs _ -> nop_node ()

let rec emit_statement s =
    match s with
    | Syntax.Define _ -> nop_node ()
    | Syntax.Assign (i, e) -> emit_assign i e
    | Syntax.If (cond, if_then, Some if_else) ->
            let _ = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let t_node = emit_statement if_then in
            let e_node = emit_statement if_else in
            let f_node = follow_node () in
            let _ = next_edge jmp_node t_node in
            let _ = jmp_edge jmp_node e_node in
            (* TODO add edge between the end of statement and f_node*)
            let _ = jmp_edge t_node f_node in
            let _ = jmp_edge e_node f_node in
            f_node
    | Syntax.If (cond, if_then, None) ->
            let _ = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let t_node = emit_statement if_then in
            let f_node = follow_node () in
            let _ = next_edge jmp_node t_node in
            (* TODO add edge between the end of statement and f_node*)
            let _ = jmp_edge t_node f_node in
            let _ = jmp_edge jmp_node f_node in
            f_node
    | Syntax.While (cond, s) ->
            let _ = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let body_node = emit_statement s in
            let f_node = follow_node () in
            let _ = next_edge jmp_node body_node in
            let _ = jmp_edge jmp_node f_node in
            (* TODO add edge between the end of statement and f_node*)
            let _ = jmp_edge body_node body_node in
            let _ = next_edge body_node f_node in
            f_node
    | Syntax.Seq (s1, s2) ->
        let n1 = emit_statement s1 in
        let n2 = emit_statement s2 in
        let _ = next_edge n1 n2 in
        n2
    | Syntax.Emp -> nop_node()

let emit_dot s =
    let _ = emit_statement s in
    let f = fun e -> let (n1, n2) = match e with 
                        | NextEdge (_, n1, n2) -> (n1, n2)
                        | ValueEdge (_, n1, n2) -> (n1, n2)
                        | JmpEdge (_, n1, n2) -> (n1, n2) in
                        Printf.printf "%s -> %s\n" (show_nodeId n1) (show_nodeId n2) in
    let _ = Printf.printf "digraph {\n" in
    let _ = List.iter f !edges in
    let _ = Printf.printf "}" in
    ()

let compile s =
    let vs = gather_vars s in
    let _ = init_vars vs in
    let _ = List.iter (fun n -> Printf.printf "%s\n" (show_node n)) !nodes in
    ()

