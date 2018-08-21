exception TypeError of string

val string_of_expr : Expression.expr -> string
val string_of_program : Expression.Program.program -> string
val typecheck_expr : Expression.expr
                  -> (string, Expression.typ) Env.env
                  -> (string, (Expression.typ list * Expression.typ)) Env.env
                  -> Expression.typ

val typecheck_prog : Expression.Program.program -> unit
