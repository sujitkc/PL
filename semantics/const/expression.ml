type expr =
  | Const of int
  | Add   of expr * expr
  | Subtract   of expr * expr

(* Interpreter *)
let rec eval e =
  match e with
  | Const(c) -> c
  | Add(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2)
      in
      i1 + i2
  | Subtract(e1, e2) ->
      let i1 = (eval e1) and i2 = (eval e2)
      in
      i1 - i2
