type expr =
  | Const of int
  | Add   of expr * expr
  | Subtract   of expr * expr
  | IfExpr of bool * expr * expr

val eval : expr -> int
