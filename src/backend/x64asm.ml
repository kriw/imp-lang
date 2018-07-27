open Extlib
exception TypeError;;
exception InvalidNode of string;;
exception TODO;;

let labels = ref []
let header = String.concat "\n" ["bits 64"; "start:"; "\n"]
let asm_entry = "mov rbp, rsp"
let asm_exit = String.concat "\n" ["mov rax, 60"; "xor rdi, rdi"; "syscall"]
let tmp1 = "rdi"
let tmp2 = "rsi"

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

let push s = Printf.sprintf "push %s" s
let pop  s = Printf.sprintf "pop %s" s
let mem2str m = Printf.sprintf "[rbp - 0x%x]" m.offset

let process_var v =
    let m = get_mem v |> mem2str in
    let s = Printf.sprintf "mov %s, %s" tmp1 m in
    String.concat "\n" [s; push tmp1]

let process_const_int i = Printf.sprintf "%d" i |> push
let process_const_bool b = if b then "1" else "0" |> push

let rec process_expr nodeId =
    let node = Cfg.find_node nodeId in
    match node with
    | Some ValueNode (_, Variable v) -> process_var v
    | Some ValueNode (_, ConstInt i) -> process_const_int i
    | Some ValueNode (_, ConstBool b) -> process_const_bool b
    | Some ActionNode (_, op) ->
        if Cfg.is_binop op then
            let line = process_binop op nodeId in
            line
        else if Cfg.is_uniop op then
            let line = process_uniop op nodeId in
            line
        else
            raise (InvalidNode (invalid_node "process_expr" nodeId))
    | _ -> raise (InvalidNode (invalid_node "process_expr" nodeId))

and process_binop op nodeId =
    let x = Cfg.find_value 0 nodeId in
    let y = Cfg.find_value 1 nodeId in
    match (x, y) with
    | (Some n1, Some n2) ->
        let x_str = process_expr n1 in
        let y_str = process_expr n2 in
        let binop_str = fun op x y ->
            String.concat "\n" [x; y; pop tmp1; pop tmp2;
            (Printf.sprintf "%s %s, %s" op tmp1 tmp2);
            (push tmp1)] in
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
            binop_str op_str x_str y_str

    | _ -> raise (InvalidNode (invalid_node "process_binop" nodeId))

and process_uniop op nodeId =
    match Cfg.find_src nodeId with
    | Some n -> raise TODO
    | _ -> raise (InvalidNode (invalid_node "process_uniop" nodeId))

let get_dst nodeId =
    let node = Cfg.find_node nodeId in
    match node with
    | Some ValueNode (_, Variable v) -> get_mem v |> mem2str
    | _ -> raise (InvalidNode (invalid_node "get_src" nodeId))

let process_assign nodeId =
    let src = Cfg.find_src nodeId in
    let dst = Cfg.find_dst nodeId in
    match (src, dst) with
    | (Some n1, Some n2) ->
        let src = process_expr n1 in
        let dst = get_dst n2 in
        String.concat "\n" [src; pop tmp1; Printf.sprintf "mov %s, %s" dst tmp1]
    | _ -> raise (InvalidNode (invalid_node "process_assign" nodeId))

let process_conditional_jmp nid =
    let cond_node = (Cfg.find_value 0 nid) |> Option.map Cfg.find_node in
    (* TODO avoid `get` *)
    let jmp_dst = Cfg.jmp_dst nid |> Option.get |> add_target_label in
    match cond_node with
    | Some Some ActionNode (nodeId, op) ->
        if Cfg.is_condop op then
            let procs =
                (* TODO avoid `get` *)
                let op1 = Cfg.find_src nodeId |> Option.get |> process_expr in
                let op2 = Cfg.find_dst nodeId |> Option.get |> process_expr in
                String.concat "\n" [op1; op2; pop tmp1; pop tmp2;
                                        Printf.sprintf "cmp %s, %s" tmp1 tmp2] in
            procs ^ match op with
            | Lt -> Printf.sprintf "jl %s" jmp_dst
            | LtEq -> Printf.sprintf "jle %s" jmp_dst
            | _ -> raise (InvalidNode (invalid_node "process_condition, invalid operator" nodeId))
        else
            raise (InvalidNode (invalid_node "process_condition is_condop is false" nodeId))
    | Some Some ValueNode (_, ConstBool b) ->
            if b then Printf.sprintf "jmp %s" jmp_dst
            else "nop"
    | _ -> raise (InvalidNode (invalid_node "process_condition" nid))

let process_jmp op nodeId =
    let dst_node = Cfg.jmp_dst nodeId in
    match dst_node with
    | Some target ->
        let dst = add_target_label target in
        Printf.sprintf "%s %s" "jmp" dst
    | _ -> raise (InvalidNode (invalid_node "process_jmp" nodeId))

let process_action nodeId =
    match Cfg.find_node nodeId with
    | Some EntryNode _ -> asm_entry
    | Some ExitNode _ -> asm_exit
    | Some ActionNode (_, op) ->
        if Cfg.is_jmp op then
            process_jmp op nodeId
        else if Cfg.is_cond_jmp op then
            process_conditional_jmp nodeId
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
    header ^ String.concat "\n" lines
