(* Environment - begin *)
type env =
    EmptyEnv
  | NonEmptyEnv of (string * Expression.type_let2) * env

let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then value
    else (apply x env')


(* Environment - end *)
