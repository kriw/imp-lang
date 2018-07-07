(* let filename = Sys.argv.(1) *)

(* let print_int_list = List.print Int.print stdout *)

let main () =
    (* let input = open_in filename in *)
    let stdinbuf = Lexing.from_channel stdin in
    (* let filebuf = Lexing.from_input input in *)
    let linebuf = Lexing.from_string (Lexer.line stdinbuf) in
    try
        (* print_int_list (Parser.main Lexer.token filebuf) *)
        (* let _ = (Parser.main Lexer.token filebuf) in *)
        let _ = (Parser.main Lexer.token linebuf) in
        ()
    with
  | Lexer.Error msg ->
          Printf.eprintf "%s%!" msg
  | Parser.Error ->
          Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
          ;
  IO.close_in input

let _ = main ()
