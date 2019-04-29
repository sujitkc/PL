(* Parser for lambda expressions - Incomplete *)

exception Parse_error of string

type token =
    IDENTIFIER of string
  | LPAREN
  | RPAREN
  | LAMBDA
(*  | EOS (* end of string *) *)

type lambda =
    Identifier of string
  | Abstraction of (string * lambda)
  | Application of (lambda * lambda)





























let rec parse_lambda lst =
  try
    parse_abstraction lst
  with
  Failure(_) ->
    try
      parse_identifier lst
    with
  Failure(_) ->
    try
      parse_application lst
    with
      Failure(_) -> failwith ""

and    parse_abstraction = function
    LAMBDA :: IDENTIFIER(s) :: t ->
      let ast, lst' = parse_lambda t in
      Abstraction(s, ast), lst'
  | _ -> failwith ""

and    parse_identifier = function
    IDENTIFIER(s) :: t ->
      Identifier(s), t
  | _ -> failwith ""

and    parse_application = function
    h :: t when h = LPAREN ->
      let ast, lst' = parse_lambda t in
      let ast', lst'' = parse_lambda lst' in
      (
        match lst'' with
          h' :: t' when h' = RPAREN ->
            Application(ast, ast'), t'
        | _ -> failwith ""
      )
  | _ -> failwith ""






































(*
  input : ""
  error : empty string
*)
let t1 () =
  let l = [ ] in
  parse_lambda l

(*
  input : "x"
*)
let t2 () =
  let l = [ IDENTIFIER("x") ] in
  parse_lambda l

(*
  input : "(x y)"
*)
let t3 () =
  let l = [ LPAREN; IDENTIFIER("x"); IDENTIFIER("y"); RPAREN ] in
  parse_lambda l

(*
  input : "lambda x (x y)"
*)
let t4 () =
  let l = [ LAMBDA; IDENTIFIER("x"); LPAREN; IDENTIFIER("x"); IDENTIFIER("y"); RPAREN ] in
  parse_lambda l

(*
  input : "lambda x (x lambda y y)"
*)
let t5 () =
  let l = [ LAMBDA; IDENTIFIER("x"); LPAREN; IDENTIFIER("x"); LAMBDA; IDENTIFIER("y"); IDENTIFIER("y"); RPAREN ] in
  parse_lambda l

(*
  input : lambda x (x lambda y y
  error : missing )
*)
let t6 () =
  let l = [ LAMBDA; IDENTIFIER("x"); LPAREN; IDENTIFIER("x"); LAMBDA; IDENTIFIER("y"); IDENTIFIER("y") ] in
  parse_lambda l
