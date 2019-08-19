(* Environment - begin *)

type ('a, 'b) env_item =
    KeyValue of 'a * 'b
  | Marker

type ('a, 'b) environment =
    EmptyEnv
  | NonEmptyEnv of ('a, 'b) env_item * ('a, 'b) environment

let empty_env () = EmptyEnv

let add_marker sigma =
  NonEmptyEnv(Marker, sigma)

let add_binding x v sigma =
  NonEmptyEnv(KeyValue(x, v), sigma)

let rec update_binding x v sigma =
  match sigma with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv(Marker, sigma') -> NonEmptyEnv(Marker, (update_binding x v sigma'))
  | NonEmptyEnv(KeyValue(key, value), sigma') ->
    if x = key then NonEmptyEnv(KeyValue(x, v), sigma')
    else NonEmptyEnv(KeyValue(key, value), (update_binding x v sigma'))

let rec apply x sigma =
  match sigma with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv(Marker, sigma') -> apply x sigma'
  | NonEmptyEnv(KeyValue(key, value), sigma') ->
    if x = key then value
    else (apply x sigma')

let exit_scope sigma =
  let rec loop  = function
      EmptyEnv -> failwith "Cannot exit scope in an empty environment."
    | NonEmptyEnv(Marker, sigma') -> sigma'
    | NonEmptyEnv(KeyValue(_, _), sigma') -> loop sigma'
  in
  loop sigma

(* Environment - end *)
