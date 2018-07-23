open Cfg
exception TypeError;;
exception InvalidOperand;;

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
    let opcode = syntax_to_opcode op in
    let node = new_action opcode in
    let _ = value_edge node x in
    let _ = value_edge node y in
    node


let rec emit_expr e =
    match e with
    | Syntax.Const c -> new_const c
    | Syntax.Ident i -> new_var i
    | Syntax.Exprs (x, op, y) ->
            let a = emit_expr x in
            let b = emit_expr y in
            emit_operator op a b

let emit_assign i e =
    let i_node = new_var i in
    let e_node = emit_expr e in
    let node = new_action Assign in
    let _ = dst_edge node i_node in
    let _ = src_edge node e_node in
    node

let last lst = List.nth lst ((List.length lst) - 1)
let rec emit_statement s =
    match s with
    | Syntax.Define _ -> [nop_node ()]
    | Syntax.Assign (i, e) -> [emit_assign i e]
    | Syntax.If (cond, if_then, Some if_else) ->
            let cond_node = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let _ = value_edge jmp_node (List.hd cond_node) in
            let t_nodes = emit_statement if_then in
            let e_nodes = emit_statement if_else in
            let f_node = follow_node () in
            let _ = next_edge jmp_node (List.hd t_nodes) in
            let _ = jmp_edge jmp_node (List.hd e_nodes) in
            let _ = jmp_edge (last t_nodes) f_node in
            let _ = jmp_edge (last e_nodes) f_node in
            [jmp_node; f_node]
    | Syntax.If (cond, if_then, None) ->
            let cond_node = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let _ = value_edge jmp_node (List.hd cond_node) in
            let t_nodes = emit_statement if_then in
            let f_node = follow_node () in
            let _ = next_edge jmp_node (List.hd t_nodes) in
            let _ = jmp_edge (last t_nodes) f_node in
            let _ = jmp_edge jmp_node f_node in
            [jmp_node; f_node]
    | Syntax.While (cond, s) ->
            let cond_node = emit_statement (Syntax.Assign (condition, cond)) in
            let jmp_node = new_action JmpIf in
            let back_jmp = new_action Jmp in
            let _ = value_edge jmp_node (List.hd cond_node) in
            let body_node = emit_statement s in
            let f_node = follow_node () in
            let _ = next_edge jmp_node (List.hd body_node) in
            let _ = jmp_edge jmp_node f_node in
            let _ = next_edge (last body_node) back_jmp in
            let _ = jmp_edge back_jmp jmp_node in
            let _ = next_edge back_jmp f_node in
            [jmp_node; f_node]
    | Syntax.Seq (s1, s2) ->
        let ns1 = emit_statement s1 in
        let n1 = last ns1 in
        let ns2 = emit_statement s2 in
        let n2 = List.hd ns2 in
        let _ = next_edge n1 n2 in
        List.append ns1 ns2
    | Syntax.Emp -> [nop_node()]

let construct_cfg s =
    let entryId = new_entry () in
    let ss = emit_statement s in
    let _ = next_edge entryId (List.hd ss) in
    let exitId = new_exit () in
    let _ = next_edge (last ss) exitId in
    entryId

let emit_dot s =
    let _ = construct_cfg s in
    let f = fun e -> let (nid1, nid2) = match e with 
                        | NextEdge (_, n1, n2) -> (n1, n2)
                        | ValueEdge (_, n1, n2) -> (n1, n2)
                        | JmpEdge (_, n1, n2) -> (n1, n2)
                        | AssignSrc (_, n1, n2) -> (n1, n2)
                        | AssignDst (_, n1, n2) -> (n1, n2) in
                    let _n1 = find_node nid1 in
                    let _n2 = find_node nid2 in
                    match (_n1, _n2) with
                    | (Some n1, Some n2) -> Printf.printf "\"%s\" -> \"%s\"\n" (show_node n1) (show_node n2)
                    | _ -> () in
    let _ = Printf.printf "digraph {\n" in
    let _ = List.iter f !edges in
    let _ = Printf.printf "}" in
    ()

let compile s =
    let vs = gather_vars s in
    let _ = init_vars vs in
    let _ = List.iter (fun n -> Printf.printf "%s\n" (show_node n)) !nodes in
    ()

