let main () =
    let stdinbuf = Lexing.from_channel stdin in
    try
        let stmt = (Parser.parse Lexer.token stdinbuf) in
        Printf.printf "%s" (Syntax.show_statement stmt)
    with
  | Lexer.Error msg ->
          Printf.eprintf "%s%!" msg
  | Parser.Error ->
          Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start stdinbuf)

let () = main ()
