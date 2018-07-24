open Extlib
exception TypeError;;
exception InvalidNode of string;;
exception TODO;;

let labels = ref []
let asm_entry = "entry"
let asm_exit = "exit"

module Vars = Map.Make(String);;

type mem = {
    size: int;
    offset: int;
    name: string;
};;

let invalid_node p nodeId = Printf.sprintf "%s @ %s" p (Cfg.show_nodeId nodeId)

let var_map = ref Vars.empty
let mems = ref []
(* Assumed size: 4, type: Int | Bool *)
let mem_size = 4
(* Offset from rbp *)
let cur_offset = ref 0
let get_mem var =
    match Vars.find_opt var !var_map with
    | Some m -> m
    | _ ->
        let new_mem = {
            size = mem_size;
            offset = !cur_offset;
            name = var;
        } in
        let _ = cur_offset := !cur_offset + mem_size in
        let _ = mems := new_mem :: !mems in
        let _ = var_map := Vars.add var new_mem !var_map in
        new_mem

let gen_label id =
    Printf.sprintf "L%d" id

let add_target_label id =
    let lbl = Printf.sprintf "L%d" id in
    let _ = labels := lbl :: !labels in
    lbl

let process_noop op nodeId =
    match op with
    | Cfg.Nop -> "nop"
    | _ -> raise (InvalidNode (invalid_node "process_noop" nodeId))

let mem2str m = Printf.sprintf "[rbp - 0x%x]" m.offset

let rec to_str nodeId =
    let node = Cfg.find_node nodeId in
    match node with
    | Some ValueNode (_, Variable v) -> (get_mem v |> mem2str, None)
    | Some ValueNode (_, ConstInt i) -> (Printf.sprintf "%d" i, None)
    | Some ValueNode (_, ConstBool b) ->
        let v = if b then "1" else "0" in
        (v, None)
    | Some ActionNode (_, op) ->
        if Cfg.is_binop op then
            let (tmp, line) = process_binop op nodeId in
            (tmp, Some line)
        else if Cfg.is_uniop op then
            let (tmp, line) = process_uniop op nodeId in
            (tmp, Some line)
        else
            raise (InvalidNode (invalid_node "to_str" nodeId))
    | _ -> raise (InvalidNode (invalid_node "to_str" nodeId))

and process_binop op nodeId =
    let x = Cfg.find_value 0 nodeId in
    let y = Cfg.find_value 1 nodeId in
    match (x, y) with
    | (Some n1, Some n2) ->
        let tmp = "tmp" in
        let (x_str, x_prevs) = to_str n1 in
        let (y_str, y_prevs) = to_str n2 in
        let binop_str = fun op x y ->
            (Printf.sprintf "mov %s, %s\n" tmp x) ^
            (Printf.sprintf "%s %s, %s\n" op tmp y) in
        let op_str = match op with
            | Cfg.Add -> "add"
            | Cfg.Sub -> "sub"
            | Cfg.Mul -> "mul"
            | Cfg.Mod -> raise TODO
            | Cfg.Div -> raise TODO
            | Cfg.Eq -> raise TODO
            | Cfg.Neq -> raise TODO
            | Cfg.And -> "and"
            | Cfg.Or -> "or"
            | Cfg.Lt -> raise TODO
            | Cfg.LtEq -> raise TODO
            | _ -> raise (InvalidNode (invalid_node "process_binop" nodeId)) in
            (tmp, binop_str op_str x_str y_str)

    | _ -> raise (InvalidNode (invalid_node "process_binop" nodeId))

and process_uniop op nodeId =
    match Cfg.find_src nodeId with
    | Some n -> raise TODO
    | _ -> raise (InvalidNode (invalid_node "process_uniop" nodeId))

let get_or_emp = Option.default "" 
let process_assign nodeId =
    let src = Cfg.find_src nodeId in
    let dst = Cfg.find_dst nodeId in
    match (src, dst) with
    | (Some n1, Some n2) ->
        let (src_str, src_prevs) = to_str n1 in
        let (dst_str, dst_prevs) = to_str n2 in
        let line = Printf.sprintf "mov %s, %s" dst_str src_str in
        (get_or_emp src_prevs) ^ (get_or_emp dst_prevs) ^ line
    | _ -> raise (InvalidNode (invalid_node "process_assign" nodeId))

let process_condition nodeId =
    let node = Cfg.find_node nodeId in
    match node with
    | Some ActionNode (_, op) ->
        if Cfg.is_binop op then
            let (tmp, _) = process_binop op nodeId in
            tmp
        else if Cfg.is_uniop op then
            let (tmp, _) = process_uniop op nodeId in
            tmp
        else if Cfg.is_assign op then
            process_assign nodeId
        else
            raise (InvalidNode (invalid_node "process_condition" nodeId))
    | _ -> raise (InvalidNode (invalid_node "process_condition" nodeId))

let process_jmp op nodeId =
    let dest_node = Cfg.jmp_dst nodeId in
    match dest_node with
    | Some target ->
        let dst = add_target_label target in
        if Cfg.is_cond_jmp op then
            let c =
                let cond_node = Cfg.find_value 0 nodeId in
                (* XXX avoid `get` *)
                process_condition (Option.get cond_node) in
            Printf.sprintf "%s %s %s" "jmpif" dst c
        else
            Printf.sprintf "%s %s" "jmp" dst
    | _ -> raise (InvalidNode (invalid_node "process_jmp" nodeId))

let process_action nodeId =
    match Cfg.find_node nodeId with
    | Some EntryNode _ -> asm_entry
    | Some ExitNode _ -> asm_exit
    | Some ActionNode (_, op) ->
        if Cfg.is_jmp op then
            let jmp_str = process_jmp op nodeId in
            jmp_str
        else if Cfg.is_assign op then
            process_assign nodeId
        else if Cfg.is_noop op then
            process_noop op nodeId
        else
            let err = Printf.sprintf "process_action Some %s" (Cfg.show_opcode op) in
            raise (InvalidNode (invalid_node err nodeId))
    | Some ValueNode _ -> ""
    | None -> ""
    (* | _ -> raise (InvalidNode "process_action _") *)

let rec compile_rec nodeId =
    let line =
        let lbl = gen_label nodeId in
        let result = process_action nodeId in
        Printf.sprintf "%s: %s" lbl result in
    let next = Cfg.next_node nodeId in
    match next with
    | Some nid -> line :: (compile_rec nid)
    | None -> [line]

let add_labels lines = raise TODO

let compile cfg =
	let lines = compile_rec cfg in
	(* let _ = add_labels lines in *)
    String.concat "\n" lines
