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
    begin
      print_string ("\n" ^ (Interpreter.string_of_program prog) ^ "\n");
      let (v, env) = (Interpreter.evaluate_prog prog) in
      begin
        match v with
          Some(e) ->  print_endline (Interpreter.string_of_expr e)
        | None -> print_endline "None"
      end;
      print_endline "sucess"
    end
  with
    End_of_file -> exit 0
  | Interpreter.TypeError(msg) -> print_string (msg ^ "\n")

let _ = evaluate ()
