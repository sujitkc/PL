(* 
  Part 1 - begin: Contains OCaml code to be directly included in the
  beginning of the generated lexer.
*)
{
exception Lexical_error of string

type token =
    Id of string
  | EOF
  | SL of string

let string_of_token = function
    Id(s) -> "Id(" ^ s ^ ")"
  | EOF   -> "eof"
  | SL(s) -> "SL(" ^ s ^ ")"
let str_lit = ref ""
}
(* Part 1 - end *)

(*
  Part 2 - begin
  Regular expression (pattern) definitions and lexing rules.
*)
let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let id = ['a'-'z''A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule scan_words = parse
  | "\""      { string_literal lexbuf }
  | id as s {  Id(s) }
  | eof     { EOF  }
  | _       { scan_words lexbuf }
and string_literal = parse
  | "\""     {
               let s = !str_lit in
               str_lit := "";
               SL(s)
             }
  | "\n"      { raise (Lexical_error "Newline in a string literal.")}
  | eof      { raise (Lexical_error "EOF within a string literal.")}
  | _ as c { str_lit := !str_lit ^ (String.make 1 c); string_literal lexbuf }

(* Part 2 - end *)

(* 
  Part 3 - begin
  Additional code.
  Typically empty when used in conjunction with a parser.
  Useful when developing a stand-alone utility with lexer
  as the front-end. For example, this application.
*)
{
let main () =
  let lexbuf = Lexing.from_channel stdin in
  let rec read_next_token token_list =
    let next_token = (scan_words lexbuf) in
      match next_token with
      | EOF -> token_list
      | _ as s  -> (read_next_token (s :: token_list))
  in
  let str_list = List.map string_of_token (List.rev (read_next_token [])) in
  List.iter print_endline str_list

let _ = main ()
}
(* Part 3 - end *)
