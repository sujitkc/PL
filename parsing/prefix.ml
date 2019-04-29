(* Parser for prefix notation *)

exception ParseError of string

type op =
    PLUS
  | DIV

type token =
    Op of op
  | NUM of int

type expr =
    Num of int
  | BinTerm of op * expr * expr

let rec string_of_list = function
    [] -> ""
  | h :: t -> h ^ "; " ^ (string_of_list t)

























let rec parse_prefix p =
  match p with
    [] -> raise (ParseError "premature end of string")
  | h :: t ->
    match h with
      Op(o) ->
        let (p', t') = parse_prefix t in
        let (p'', t'') = parse_prefix t' in
        (BinTerm(o, p', p''), t'')
    | NUM(n) ->
        (Num(n), t)
















let string_of_op = function
    PLUS -> "+"
  | DIV  -> "/"

let rec infix_of_exp = function
    Num(n) -> string_of_int n
  | BinTerm(o, e1, e2) ->
      "(" ^ (infix_of_exp e1) ^
      (string_of_op o) ^
      (infix_of_exp e2) ^ ")"

let rec evaluate = function
    Num(n) -> n
  | BinTerm(o, e1, e2) ->
    (
      match o with
        DIV -> (evaluate e1) / (evaluate e2)
      | PLUS -> (evaluate e1) + (evaluate e2)
    )




























let t e =
  let (ast, _) = parse_prefix e in
  let infix = infix_of_exp ast in (print_endline ("infix = " ^ infix));
  let value = evaluate ast in (print_endline ("value = " ^ (string_of_int value)))


let t1 () =
  let e = [Op(PLUS); NUM(1); NUM(2)] in (t e)
