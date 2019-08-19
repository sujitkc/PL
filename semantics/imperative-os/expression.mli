type typ =
    Boolean
  | Integer

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
  | FunCall   of string * expr list
  | Closure   of string list * stmt * (string, expr) Environment.environment * (string, expr) Environment.environment  (* (par, body, env, fenv) *)
and stmt =
  | Assignment of string * expr
  | Return of expr
  | Block of (string * typ) list * stmt list

type fundef = {
  fname  : string;
  rtype  : typ;
  params : (string * typ) list;
  body   : stmt;
}

type program = {
  decls    : (string * typ) list;
  fundefs : fundef list;
  statement : stmt;
}
