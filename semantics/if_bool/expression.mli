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

val eval : expr -> int

val bool_eval : bool_expr -> bool
