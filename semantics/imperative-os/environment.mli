type ('a, 'b) env_item =
    KeyValue of 'a * 'b
  | Marker

type ('a, 'b) environment =
    EmptyEnv
  | NonEmptyEnv of ('a, 'b) env_item * ('a, 'b) environment

val empty_env : unit -> ('a, 'b) environment

val add_marker : ('a, 'b) environment -> ('a, 'b) environment

val add_binding : 'a -> 'b -> ('a, 'b) environment -> ('a, 'b) environment

val apply : 'a -> ('a, 'b) environment -> 'b

val update_binding : 'a -> 'b -> ('a, 'b) environment -> ('a, 'b) environment

val exit_scope : ('a, 'b) environment -> ('a, 'b) environment
