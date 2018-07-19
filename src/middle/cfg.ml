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

