let read_statement () =
    let stdinbuf = Lexing.from_channel stdin in
    try
        let stmt = (Parser.parse Lexer.token stdinbuf) in
        Some stmt
    with
  | Lexer.Error msg ->
          let _ = Printf.eprintf "%s%!" msg in
          None
  | Parser.Error ->
          let _ = Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start stdinbuf) in
          None

let interpreter () =
    match read_statement () with
    | Some stmt ->
        let _ = Printf.printf "%s\n" (Syntax.show_statement stmt) in
        let _ = Interpreter.eval_statement stmt in
        let _ = Printf.printf "Env:\n" in
        Interpreter.Env.iter (fun k v -> Printf.printf "%s = %s\n" k (Syntax.show_const v)) !Interpreter.env
    | None -> Printf.eprintf "Failed to parse"

let ir () = 
    match read_statement () with
    | Some stmt -> Ir.compile stmt
    | None -> ()

let ir_dot () = 
    match read_statement () with
    | Some stmt -> Ir.emit_dot stmt
    | None -> ()

let compiler () =
    match read_statement () with
    | Some stmt ->
        let entry = Ir.construct_cfg stmt in
        Printf.printf "%s\n" (X64asm.compile entry)
    | None -> ()


let () = ir ()
(* let () = compiler () *)
