exception TypeError of string

val getIntConstValue : Expression.expr -> int
val getBoolConstValue : Expression.expr -> bool

val string_of_expr : Expression.expr -> string
val eval : Expression.expr -> Env.env -> Expression.expr
