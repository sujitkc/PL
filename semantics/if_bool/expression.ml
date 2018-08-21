type bool_expr =
  | Boolean of bool
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr

type expr =
  | Const of int
  | Add   of expr * expr
  | Subtract   of expr * expr
  | IfExpr of bool_expr * expr * expr

(* Interpreter *)
let rec bool_eval e =
  match e with
  | Boolean(b) -> b
  | And(b1, b2) -> (bool_eval b1) && (bool_eval b2)
  | Or(b1, b2) -> (bool_eval b1) || (bool_eval b2)
  | Not(b) -> not (bool_eval b)

let rec eval e =
  match e with
  | Const(c) -> c
  | Add(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2) in
      i1 + i2
  | Subtract(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2) in
      i1 - i2
  | IfExpr(b, e1, e2) -> if (bool_eval b) = true then (eval e1) else (eval e2)

