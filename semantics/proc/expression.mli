type expr =
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | Sub       of expr * expr
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | BoolConst of bool
  | Not       of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Equals    of expr * expr
  | Closure   of string * expr * env
    (* Corresponds to the underlying function closure. Note that it is a normal form as IntConst and BoolConst. *)
  | FunDef    of string * expr
    (* The abstract syntactic structure. This doesn't have the information about the environment. *)
  | FunApp    of expr * expr
    (* Application of function *)
and env =
    EmptyEnv
  | NonEmptyEnv of (string * expr) * env

val string_of_expr : expr -> string

val emptyEnv : unit -> env

val addBinding : string -> expr -> env -> env

val apply : string -> env -> expr
