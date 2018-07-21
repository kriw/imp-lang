exception TypeError;;
exception InvalidNode;;
exception TODO;;

let labels = ref []
let asm_entry = "entry"


let add_target_label id =
    let lbl = Printf.sprintf "L%d" id in
    let _ = labels := lbl :: !labels in
    lbl

let process_uniop op nodeId =
    match Cfg.find_src nodeId with
    | Some n -> raise TODO
    | _ -> raise InvalidNode

let process_binop op nodeId =
    let src = Cfg.find_src nodeId in
    let dst = Cfg.find_dst nodeId in
    match (src, dst) with
    | (Some n1, Some n2) -> raise TODO
    | _ -> raise InvalidNode

let process_jmp op nodeId =
    let dest_node = Cfg.jmp_dst nodeId in
    match dest_node with
    | Some target -> add_target_label target
    | _ -> raise InvalidNode

let process_action nodeId =
    match Cfg.find_node nodeId with
    | Some EntryNode -> asm_entry
    | Some ActionNode (_, op) ->
        if Cfg.is_jmp op then
            process_jmp op nodeId
        else if Cfg.is_binop op then
            process_binop op nodeId
        else if Cfg.is_uniop op then
            process_uniop op nodeId
        else
            raise InvalidNode
    | _ -> raise InvalidNode

let rec compile_rec nodeId =
    let result = process_action nodeId in
    let next = Cfg.next_node nodeId in
    match next with
    | Some nid -> result :: (compile_rec nid)
    | None -> []

let add_labels lines = raise TODO

let compile cfg =
	let lines = compile_rec cfg in
	let _ = add_labels lines in
	()
