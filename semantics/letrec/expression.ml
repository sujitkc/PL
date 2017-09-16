(* Expression - begin *)
type expr =
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | Sub       of expr * expr
  | Multiply  of expr * expr
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | BoolConst of bool
  | Not       of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Equals    of expr * expr
  | Closure   of string * expr * env
  | FunDef    of string * expr
  | FunApp    of expr * expr
  | RecFunDef   of string * expr
  | RecClosure  of string * expr * env

(* Expression - end *)
(* Environment - begin *)
and env =
    EmptyEnv
  | NonEmptyEnv of (string * expr) * env
(* Environment - end *)

let rec string_of_expr = function
  | Id(vname)             -> vname
  | IntConst(n)           -> string_of_int n
  | Add(e1, e2)           -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Sub(e1, e2)           -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | Multiply(e1, e2)      -> (string_of_expr e1) ^ " * " ^ (string_of_expr e2)
  | If(b, e1, e2)         -> "if (" ^ (string_of_expr b) ^ ") then (" ^ (string_of_expr e1) ^ ") else (" ^ (string_of_expr e2) ^ ")"
  | Let(vname, e1, e2)    -> "let " ^ vname ^ " = (" ^ (string_of_expr e1) ^ ") in (" ^ (string_of_expr e2) ^ ")"
  | BoolConst(b)          -> string_of_bool b
  | Not(e)                -> "not(" ^ string_of_expr e ^ ")"
  | Or(e1, e2)            -> "(" ^ (string_of_expr e1) ^ ") or (" ^ (string_of_expr e2) ^ ")"
  | And(e1, e2)           -> "(" ^ (string_of_expr e1) ^ ") and (" ^ (string_of_expr e2) ^ ")"
  | Equals(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"
  | Closure(vname, e, env)
  | RecClosure(vname, e, env) ->  "(fun " ^ vname ^ "->" ^ (string_of_expr e) ^") " ^ (string_of_env env)(*failwith "no string representation for closure." *)  
  | FunDef(vname, e)      -> "fun " ^ vname ^ " -> " ^ (string_of_expr e)
  | FunApp(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") (" ^ (string_of_expr e2) ^ ")"
  | RecFunDef(vname, e)      -> "fun " ^ vname ^ " -> " ^ (string_of_expr e)   
and string_of_env env =
  let rec iter = function
      EmptyEnv -> ""
    | NonEmptyEnv((vname, value), env') -> "(" ^ vname ^ "=" ^ (string_of_expr value) ^ "); " ^ (string_of_env env')
  in
  "[" ^ (iter env) ^ "]" 

let getIntConstValue e =
  match e with
    IntConst(c) -> c
  | _        -> raise (Failure (("getIntConstValue: The expression is not in IntConst normal form.") ^ (string_of_expr e)))
 
let getBoolConstValue e =
  match e with
    BoolConst(b) -> b
  | _            -> raise (Failure (("getBoolConstValue: The expression is not in BoolConst normal form.") ^ (string_of_expr e)))

(* This is the new addition to our extractor functions. The only difference here is that it returns a tuple,
   as the normal form Closures 3 and not 1 underlying value. *)
let getClosureValue e =
  match e with
    Closure(par, body, env)    -> (par, body, env)
  | RecClosure(par, body, env)    -> (par, body, env)
  | _            -> raise (Failure "getClosureValue: The expression is not in FunClosure normal form.")



let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then
    (
      match value with
        | RecClosure(_, _, _) ->
            let (par, body, env'') = getClosureValue(value) in
            let env''' = (addBinding vname (RecClosure(par, body, env'')) env'') in
              Closure(par, body, env''')
        | _                   -> value
    )
    else (apply x env')
