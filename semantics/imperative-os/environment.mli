type ('a, 'b) env_item =
    KeyValue of 'a * 'b
  | Marker

type ('a, 'b) environment =
    EmptyEnv
  | NonEmptyEnv of ('a, 'b) env_item * ('a, 'b) environment

val empty_env : unit -> ('a, 'b) environment

(* Does the necessary book-keeping tasks while entering a block (scope) *)
val enter_scope : ('a, 'b) environment -> ('a, 'b) environment

(* Adds a new binding to the environment. Useful for let bindings. *)
val add_binding : 'a -> 'b -> ('a, 'b) environment -> ('a, 'b) environment

(* Retrieves the value associated with the given variable from the environment. *)
val apply : 'a -> ('a, 'b) environment -> 'b

(* Returns a new environment with the binding of the variable updated to the
 new value. Used by assignment statements. *)
val update_binding : 'a -> 'b -> ('a, 'b) environment -> ('a, 'b) environment

val exit_scope : ('a, 'b) environment -> ('a, 'b) environment
