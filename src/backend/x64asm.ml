exception TypeError;;
exception InvalidNode;;
exception TODO;;

let process_uniop op src = raise TODO
let process_binop op src dst = raise TODO

let process_action nodeId =
    let node = Cfg.find_node nodeId in
    match node with
    | Some EntryNode -> raise TODO
    | Some ActionNode (_, op) ->
        if Cfg.is_binop op then
            let src = Cfg.find_src nodeId in
            let dst = Cfg.find_dst nodeId in
            match (src, dst) with
            | (Some n1, Some n2) -> process_binop op n1 n2
            | _ -> raise InvalidNode
        else if Cfg.is_uniop op then
            let src = Cfg.find_src nodeId in
            match src with
            | Some n -> process_uniop n
            | _ -> raise InvalidNode
        else
            raise InvalidNode
    | _ -> raise InvalidNode

let rec compile_rec nodeId =
    let result = process_action nodeId in
    let next = Cfg.next_node nodeId in
    match next with
    | Some nid -> result :: (compile_rec nid)
    | None -> []

let compile cfg = ()
