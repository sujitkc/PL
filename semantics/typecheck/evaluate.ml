let evaluate () =
  try
    let cin =
      if Array.length Sys.argv > 1 then
        open_in Sys.argv.(1)
      else
        stdin
    in
    let e1 =
      let lexbuf = Lexing.from_channel cin in
        Parser.expr Lexer.scan lexbuf
    in
    print_string ("\n" ^ (Interpreter.string_of_expr e1) ^ "\n");
    let result = (Interpreter.eval e1 Env.EmptyEnv) in
    match result with
      Expression.Integer -> Printf.printf "\n\t = Integer\n"
    | Expression.Boolean -> Printf.printf "\n\t = Boolean\n"
  with
    End_of_file -> exit 0
  | Interpreter.TypeError(msg) -> print_string (msg ^ "\n")

let _ = evaluate ()
