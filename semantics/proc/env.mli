val string_of_expr : expr -> string

val emptyEnv : unit -> env

val addBinding : string -> expr -> env -> env

val apply : string -> env -> expr
