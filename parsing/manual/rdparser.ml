type expr =
    Num      of int
  | Add      of expr * expr
  | Subtract of expr * expr
  | Paren    of expr

(*
Grammar:

  expr  : term             (* expr2   *)

  term  : factor           (* term1   *)
        | factor ADD term  (* term2   *)

  factor: NUM              (* factor1 *)
        | NUM * factor     (* factor2 *)
*)

(*
Grammar with left recursion removed:

  expr    : term

  term    : factor term'

  term'   : EOF
          | ADD term  

  factor  : NUM factor'

  factor' : EOF
          | * factor
*)

let next_token = ref Lexer.EOF

(* match function - named 'expect' as match is an OCaml keyword. *)
let expect (tok : Lexer.token) =
  match tok with
    Lexer.NUM(_) ->
    (
      match !next_token with
        Lexer.NUM(_) -> true
      | _ -> false
    )
  | _ -> tok = !next_token

let consume lexbuf =
  next_token := Lexer.scan lexbuf

let rec expr lexbuf =
  consume lexbuf;
  term lexbuf

and term lexbuf =
    if (factor lexbuf) = false then false
    else
    begin
      term' lexbuf
    end

and term' lexbuf =
  if (expect Lexer.EOF) then true
  else if (expect Lexer.ADD) then
  begin
    consume lexbuf;
    term lexbuf
  end
  else false

and factor lexbuf =
  if (expect (Lexer.NUM(1))) then
  begin
    consume lexbuf;
    factor' lexbuf
  end
  else false

and factor' lexbuf =
  if (expect Lexer.EOF) then true
  else if (expect Lexer.MUL) then
  begin
    consume lexbuf;
    factor lexbuf
  end
  else if (expect Lexer.ADD) then true
  else false

let tss = [
  "1"        ;
  "1 + 2"    ;
  "1 + 2 + 3";
  "1 + 2 + " ; (* <error> *)
  "1 * 2"    ;
  "1 + 2 * 3";
  "1 * 2 + 3";
  "1 2"      ; (* <error> *)
  "1 * 2 + " ; (* <error> *)
  "1 + 2 * " ; (* <error> *)
]

let test_rdparser s =
  let lexbuf = Lexing.from_string s in
  Printf.printf "%s -> %b\n" s (expr lexbuf)

let test_all () =
  let rec iter = function
      [] -> ()
    | h :: t -> (test_rdparser h); (iter t)
  in
  iter tss

let _ = test_all ()
