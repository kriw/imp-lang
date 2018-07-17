let main () =
    let stdinbuf = Lexing.from_channel stdin in
    try
        let stmt = (Parser.parse Lexer.token stdinbuf) in
        let _ = Printf.printf "%s\n" (Syntax.show_statement stmt) in
        let _ = Interpreter.eval_statement stmt in
        let _ = Printf.printf "Env:\n" in
        Interpreter.Env.iter (fun k v -> Printf.printf "%s = %s\n" k (Syntax.show_const v)) !Interpreter.env
    with
  | Lexer.Error msg ->
          Printf.eprintf "%s%!" msg
  | Parser.Error ->
          Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start stdinbuf)

let () = main ()
