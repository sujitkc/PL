type expr_type =
    Num of int
  | Add of expr_type * expr_type
  | Mul of expr_type * expr_type

val string_of_expr : expr_type -> string
val val_of_expr : expr_type -> int
