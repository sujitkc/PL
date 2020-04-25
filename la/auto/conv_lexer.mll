(* 
  Part 1 - begin: Contains OCaml code to be directly included in the
  beginning of the generated lexer.
*)
{
exception Lexer_exception of string

type token =
    Id of string
  | EOF
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
  | id as s {  Id(s) }
  | eof     { EOF  }
  | _       { scan_words lexbuf }
(* Part 2 - end *)

(* 
  Part 3 - begin
  Additional code.
  Typically empty when used in conjunction with a parser.
  Useful when developing a stand-alone utility with lexer
  as the front-end. For example, this application.
*)

{
let print_table t =
  (Hashtbl.iter (fun key value ->
    (Printf.printf "%s %d\n" key !value)) t)

let print_list l p = print_newline (); List.iter p l

let sort_table t =
  let assoc_list = (Hashtbl.fold
                     (fun k v acc -> (k, !v) :: acc) t []) in
    List.sort
      (
        fun (k1, v1) (k2, v2) ->
          if v1 > v2 then 1 else if v1 < v2 then -1 else 0
      ) assoc_list

let reject_words = [ "let"; "the"; "is"; "for"; "I"; "in"; "on"; "upon";
  "between"; "under"; "above"; "over";
  "as"; "at"; "be"; "an"; "a"; "the"; "from"; "to"; "upto"; "am"; "was";
  "would"; "could"; "should"; "shall" ]

let reject_words_table =
  let table = Hashtbl.create 30 in
  let rec create_reject_words_table = function
      [] -> table
    | h :: t -> (Hashtbl.add table h ()); create_reject_words_table t
  in
  create_reject_words_table reject_words
    

let read_all_words fin =
  let lexbuf = Lexing.from_channel fin in
  let rec read_next_word word_list =
    let next_token = (scan_words lexbuf) in
      match next_token with
          Id(s)  ->
            if not (Hashtbl.mem reject_words_table s) then
              (read_next_word (s :: word_list))
            else
              read_next_word word_list
        | EOF -> word_list
  in
  (List.rev (read_next_word []))

(*
  Return a hash table with key = word, value = number of occurance.
*)
let make_table word_list =
  let table = Hashtbl.create 30 in
  (* Enter one word. Increment count by 1 if already there; else, create entry *)
  let rec enter_word = function
      h :: t ->
        if Hashtbl.mem table h then
          let count = Hashtbl.find table h in count := !count + 1
        else 
          Hashtbl.add table h (ref 1);
          enter_word t
    | [] -> ()
  in
  enter_word word_list;
  table

let main () =
  let fin =
    if Array.length Sys.argv = 1 then
      stdin
    else
      open_in Sys.argv.(1)
  in
  let word_list = read_all_words fin in
  let table = make_table word_list
  in
  print_table table;
  let sorted = (sort_table table) in
    (print_list sorted (fun (k, v) -> (Printf.printf "%s -> %d\n" k v)))
  ;;

main ()
}
(* Part 3 - end *)
