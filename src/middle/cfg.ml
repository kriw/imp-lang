open Extlib
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

let is_noop op =
    match op with
    | Nop -> true
    | _ -> false

let is_binop op =
    match op with
    | Assign -> true
    | JmpIf -> false
    | Jmp -> false
    | Add -> true
    | Sub -> true
    | Mul -> true
    | Mod -> true
    | Div -> true
    | Eq -> true
    | Neq -> true
    | And -> true
    | Or -> true
    | Lt -> true
    | LtEq -> true
    | Nop -> false

(*  TODO *)
let is_uniop op = false

let is_jmp op =
    match op with
    | Jmp -> true
    | JmpIf -> true
    | _ -> false

let syntax_to_opcode op =
    match op with
    | Syntax.Add -> Add
    | Syntax.Sub -> Sub
    | Syntax.Mul -> Mul
    | Syntax.Mod -> Mod
    | Syntax.Div -> Div
    | Syntax.Eq  -> Eq
    | Syntax.Neq -> Neq
    | Syntax.Lt  -> Lt
    | Syntax.LtEq -> LtEq
    | Syntax.And -> And
    | Syntax.Or  -> Or

type nodeId = int
[@@deriving show]

type edgeId = int
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
    | AssignSrc of edgeId * nodeId * nodeId
    | AssignDst of edgeId * nodeId * nodeId
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

let new_edge constructor =
    let eid = new_edge_id () in
    let _ = constructor eid |> add_edge in
    eid

let next_edge from_node to_node =
    new_edge (fun i -> NextEdge (i, from_node, to_node))

let jmp_edge from_node to_node =
    new_edge (fun i -> JmpEdge (i, from_node, to_node))

let value_edge from_node to_node =
    new_edge (fun i -> ValueEdge (i, from_node, to_node))

let src_edge from_node to_node =
    new_edge (fun i -> AssignSrc (i, from_node, to_node))

let dst_edge from_node to_node =
    new_edge (fun i -> AssignDst (i, from_node, to_node))

let new_node constructor =
    let nid = new_node_id () in
    let _ = constructor nid |> add_node in
    nid

(* Assumed EntryNode is already allocated *)
let entry () = new_node_id ()

let new_var name =
    new_node (fun i -> ValueNode (i, Variable name))

let new_action action =
    new_node (fun i -> ActionNode (i, action))

let new_const const =
    let node = match const with
                | Syntax.Int i -> ConstInt i
                | Syntax.Bool b -> ConstBool b in
    new_node (fun i -> ValueNode (i, node))

let nop_node () =
    new_node (fun i -> ActionNode (i, Nop))

(* Follow node of if statement *)
let follow_node = nop_node

let filter_map f lst =
    List.map f lst
    |> List.filter Option.is_some
    |> List.map Option.get

let hd_opt lst =
    match lst with
    | n :: _ -> Some n
    | _ -> None

let take1 f lst = hd_opt (filter_map f lst)

let next_node nodeId =
    let filter e = match e with
                | NextEdge (_, i, target) when i == nodeId -> Some target
                | JmpEdge (_, i, target) when i == nodeId -> Some target
                | ValueEdge (_, i, target) when i == nodeId -> Some target
                | _ -> None in
    take1 filter !edges

let find_src nodeId =
    let filter e = match e with
                | AssignSrc (_, i, target) when i == nodeId -> Some target
                | _ -> None in
    take1 filter !edges

let find_dst nodeId =
    let filter e = match e with
                | AssignDst (_, i, target) when i == nodeId -> Some target
                | _ -> None in
    take1 filter !edges


let jmp_dst nodeId =
    let filter e = match e with
                | JmpEdge (_, i, target) when i == nodeId -> Some target
                | _ -> None in
    take1 filter !edges
