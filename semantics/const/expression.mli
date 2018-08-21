type expr =
  | Const of int
  | Add   of expr * expr
  | Subtract   of expr * expr

val eval : expr -> int
