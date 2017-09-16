type bool_expr =
  | Boolean of bool
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr

type expr =
  | Id       of string
  | Const    of int
  | Add      of expr * expr
  | Subtract of expr * expr
  | IfExpr   of bool_expr * expr * expr
  | LetExpr  of string * expr * expr

val eval : expr -> Env.env -> int

val bool_eval : bool_expr -> bool
