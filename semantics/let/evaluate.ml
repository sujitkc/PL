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
    Printf.printf "\n\t = %d\n" (Expression.eval e1 Env.EmptyEnv)
  with End_of_file -> exit 0

let _ = evaluate ()
