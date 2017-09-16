type expr_type =
    Num of int
  | Add of expr_type * expr_type
  | Mul of expr_type * expr_type

let rec string_of_expr = function
    Num(n) -> string_of_int n
  | Add(l, r) -> (string_of_expr l) ^ " + " ^ (string_of_expr r)
  | Mul(l, r) -> (string_of_expr l) ^ " * " ^ (string_of_expr r)

let rec val_of_expr = function
    Num(n) -> n
  | Add(l, r) -> (val_of_expr l) +  (val_of_expr r)
  | Mul(l, r) -> (val_of_expr l) * (val_of_expr r)
