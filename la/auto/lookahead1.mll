(* 
  Part 1 - begin: Contains OCaml code to be directly included in the
  beginning of the generated lexer.
*)
{
exception Lexical_error

type token =
    EQ3
  | EQ2
  | EQ1
  | EOF
}
(* Part 1 - end *)

(*
  Part 2 - begin
  Regular expression (pattern) definitions and lexing rules.
*)

rule scan_words = parse
  | "=======" { EQ3                 }
  | "===="    { EQ2                 }
  | "=="      { EQ1              }
  | eof      { EOF                 }
  | _        { raise Lexical_error }
(* Part 2 - end *)

(* 
  Part 3 - begin
  Additional code.
  Typically empty when used in conjunction with a parser.
  Useful when developing a stand-alone utility with lexer
  as the front-end. For example, this application.
*)

{

let print_token = function
    EQ3 -> print_endline "eq3"
  | EQ2 -> print_endline "eq2"
  | EQ1 -> print_endline "eq1"
  | EOF -> print_endline "end of file"

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let rec loop tlist =
    let next_token = (scan_words lexbuf) in
      match next_token with
        | EOF -> tlist
        | _   -> loop (next_token :: tlist)
  in
  let tokens = (List.rev (loop [])) in
  List.iter print_token tokens 

let _ = main ()
}
(* Part 3 - end *)
