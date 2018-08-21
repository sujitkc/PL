let evaluate () =
  try
    let cin =
      if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin
    in
    let prog =
      let lexbuf = Lexing.from_channel cin in
        Parser.prog Lexer.scan lexbuf
    in
    print_string ("\n" ^ (Interpreter.string_of_program prog) ^ "\n");
    let _ = (Interpreter.typecheck_prog prog) in
    print_string "sucess"
  with
    End_of_file -> exit 0
  | Interpreter.TypeError(msg) -> print_string (msg ^ "\n")

let _ = evaluate ()
