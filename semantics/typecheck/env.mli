type env =
    EmptyEnv
  | NonEmptyEnv of (string * Expression.type_let2) * env

val emptyEnv : unit -> env

val addBinding : string -> Expression.type_let2 -> env -> env

val apply : string -> env -> Expression.type_let2
