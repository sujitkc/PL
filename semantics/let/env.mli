type env =
    EmptyEnv
  | NonEmptyEnv of (string * int) * env

val emptyEnv : unit -> env

val addBinding : string -> int -> env -> env

val apply : string -> env -> int
