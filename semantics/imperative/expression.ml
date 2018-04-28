type typ =
    Boolean
  | Integer

(* Expression - begin *)
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
(* Expression - end *)

module Program = struct
  type stmt =
    | Assignment of string * expr
    | Return of expr
    | Block of stmt list

  type fundef = {
    fname  : string;
    rtype  : typ;
    params : (string * typ) list;
    body   : stmt;
  }

  type program = {
    decls    : (string * typ) list;
    fundefs : fundef list;
  }
end

(*
module Program : ProgramType = struct
  type stmt =
    | Assignment of string * expr

  type fundef = {
    fname  : string;
    rtype  : typ;
    params : (string * typ) list;
    body   : stmt;
  }

  type program = {
    defs    : (string * typ) list;
    fundefs : fundef list;
  }
end
*)

