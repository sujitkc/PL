exception TypeError of string

val string_of_expr : Expression.expr -> string
val string_of_program : Expression.program -> string
val evaluate_expr : Expression.expr
                  -> (string, Expression.expr) Environment.environment
                  -> (string, Expression.expr) Environment.environment
                  -> Expression.expr option * ((string, Expression.expr) Environment.environment)

val evaluate_prog : Expression.program ->
           Expression.expr option *
           (string, Expression.expr) Environment.environment


