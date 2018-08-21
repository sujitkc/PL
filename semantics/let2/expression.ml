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
(* Expression - end *)

