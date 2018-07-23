exception TypeError;;
exception InvalidNode of string;;
exception TODO;;

let labels = ref []
let asm_entry = "entry"
let asm_exit = "exit"


let add_target_label id =
    let lbl = Printf.sprintf "L%d" id in
    let _ = labels := lbl :: !labels in
    lbl

let process_noop op nodeId =
    match op with
    | Cfg.Nop -> "nop"
    | _ -> raise (InvalidNode "process_noop")

let process_uniop op nodeId =
    match Cfg.find_src nodeId with
    | Some n -> raise TODO
    | _ -> raise (InvalidNode "process_uniop")

    (* TODO *)
let to_str n = "rax"

let process_binop op nodeId =
    let src = Cfg.find_src nodeId in
    let dst = Cfg.find_dst nodeId in
    match (src, dst) with
    | (Some n1, Some n2) ->
        let src_str = to_str src in
        let dst_str = to_str dst in
        let op_str = match op with
        | Cfg.Assign -> "mov"
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
        | _ -> raise TODO in
        Printf.sprintf "%s %s, %s" op_str src_str dst_str
    | _ -> raise (InvalidNode "process_binop")

let process_jmp op nodeId =
    let dest_node = Cfg.jmp_dst nodeId in
    match dest_node with
    | Some target ->
        let _ = add_target_label target in
        (* TODO *)
        "jmp"
    | _ -> raise (InvalidNode "process_jmp")

let process_action nodeId =
    match Cfg.find_node nodeId with
    | Some EntryNode _ -> asm_entry
    | Some ExitNode _ -> asm_exit
    | Some ActionNode (_, op) ->
        if Cfg.is_jmp op then
            let jmp_str = process_jmp op nodeId in
            jmp_str
        else if Cfg.is_binop op then
            process_binop op nodeId
        else if Cfg.is_uniop op then
            process_uniop op nodeId
        else if Cfg.is_noop op then
            process_noop op nodeId
        else
            let err = Printf.sprintf "process_action Some %s" (Cfg.show_opcode op) in
            raise (InvalidNode err)
    | Some ValueNode _ -> ""
    | None -> ""
    (* | _ -> raise (InvalidNode "process_action _") *)

let rec compile_rec nodeId =
    let result = process_action nodeId in
    let next = Cfg.next_node nodeId in
    match next with
    | Some nid -> result :: (compile_rec nid)
    | None -> [result]

let add_labels lines = raise TODO

let compile cfg =
	let lines = compile_rec cfg in
	(* let _ = add_labels lines in *)
    String.concat "\n" lines
