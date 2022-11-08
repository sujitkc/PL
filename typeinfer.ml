exception TypeError of string

type expr =
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | BoolConst of bool
  | Not       of expr
  | And       of expr * expr
  | Equals    of expr * expr
  | FunApp    of expr * expr
  | Lambda    of string * expr

type definition =
  | FunDef of string * string * expr
  | VarDef of string * expr

type type_expr =
    BasicType    of string
  | UnaryOp      of string * type_expr
  | BinOp        of string * type_expr * type_expr
  | TypeVariable of string

and polymorphic_type_expr =
    ForAll of string * polymorphic_type_expr
  | TypeExpr of type_expr

let rec string_of_type_expr = function
    BasicType(s)     -> s
  | UnaryOp(s, t)    -> s ^ " ( " ^ (string_of_type_expr t) ^ " ) "
  | BinOp(s, t1, t2) -> s ^ " ( " ^ (string_of_type_expr t1) ^ " , " ^ (string_of_type_expr t2) ^ " ) "
  | TypeVariable(s)  -> s

and string_of_polymorphic_type_expr = function
    ForAll(s, pte) -> "(forall " ^ s ^ (string_of_polymorphic_type_expr pte) ^ " )"
  | TypeExpr(te)   -> string_of_type_expr te

let rec string_of_expr = function
  | Id(vname)             -> vname
  | IntConst(n)           -> string_of_int n
  | Add(e1, e2)           -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | If(b, e1, e2)         -> "if (" ^ (string_of_expr b) ^ ") then (" ^ (string_of_expr e1) ^ ") else (" ^ (string_of_expr e2) ^ ")"
  | Let(vname, e1, e2)    -> "let " ^ vname ^ " = (" ^ (string_of_expr e1) ^ ") in (" ^ (string_of_expr e2) ^ ")"
  | BoolConst(b)          -> string_of_bool b
  | Not(e)                -> "not(" ^ string_of_expr e ^ ")"
  | And(e1, e2)           -> "(" ^ (string_of_expr e1) ^ ") and (" ^ (string_of_expr e2) ^ ")"
  | Equals(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"
  | FunApp(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") (" ^ (string_of_expr e2) ^ ")"
  | Lambda(vname, e)      -> "fun (" ^ (string_of_expr e) ^ ")" 

module Type = struct
  let empty_env () = []

  let add_binding x v env =
    (x, v) :: env

  let rec lookup x env =
    match env with
      [] -> raise Not_found
    | (vname, value) :: env' ->
      if x = vname then value
      else (lookup x env')

  let rec string_of_env env =
    let rec iter = function
        [] -> ""
      | (vname, value) :: env' -> "(" ^ vname ^ (string_of_type_expr value) ^ "); " ^ (string_of_env env')
    in
    "[" ^ (iter env) ^ "]"
end


(*
  Given a type expression and a map from identifier names i1 to names i2,
  returns an equivalent type expression with the all occurrances of i1
  replaced by i2. The function goes recursively down the type expression.
*)
let rec replace map = function
    BasicType(s)     -> BasicType(s)
  | UnaryOp(s, t)    -> UnaryOp(s, (replace map t))
  | BinOp(s, t1, t2) -> BinOp(s, (replace map t1), (replace map t2))
  | TypeVariable(s)  ->
      (
        try
          (TypeVariable (Hashtbl.find map s))
        with Not_found ->
          TypeVariable(s)
      )

(*
  Given a polymorphic type expression, returns the outermost non-polymorphic
  type expression contained within it.
*)
let rec get_type_expr = function
    ForAll(s, pte') -> get_type_expr pte'
  | TypeExpr(t) -> t

(*
  Take a polymorphic type expression and returns a type expression with
    all its quantified variables replaced by new type variables.
  Remark: Has a side-effect on table. Adds an entry corresponding to the newly created
    type variable in table.
*) 
let instantiate_polymorphic_type_expr t table =
  let map = Hashtbl.create 30 in
  let rec add_entry pte =
    let new_type_name () = "t" ^ (string_of_int (Hashtbl.length table)) in
    match pte with
      ForAll(s, pte') ->
        let name = new_type_name () in
        let type_var = TypeVariable(name) in
        Hashtbl.add map s name;
        Hashtbl.add table type_var ((Hashtbl.length table), type_var);
        add_entry pte'
    | TypeExpr(_) -> ()
  in
  add_entry t;
  replace map (get_type_expr t)


let union t1 t2 table =
  let (s1, t) = Hashtbl.find table t1
  and (s2, _) = Hashtbl.find table t2 in
  Hashtbl.iter
  (
    fun a b ->
      let (set_num, _) = Hashtbl.find table a in
      if set_num = s2 then
        (Hashtbl.replace table a (s1, t))
      else ()
  ) table

let find t table =
  Hashtbl.find table t

let rec unify m n table =
  print_endline ("unifying " ^ (string_of_type_expr m) ^ " and " ^
    (string_of_type_expr n));
  let s = find m table
  and t = find n table in
  if s = t then true
  else
    match m, n with
      BasicType(t1), BasicType(t2) when t1 = t2                 ->
        true
    | UnaryOp(str1, s'), UnaryOp(str2, t') when str1 = str2     ->
        (union m n table);
        (unify s' t' table)
    | BinOp(str1, s1, s2), BinOp(str2, t1, t2) when str1 = str2 ->
        (union m n table);
        (unify s1 t1 table) && (unify s2 t2 table)
    | TypeVariable(x), TypeVariable(y)                          ->
        (union m n table);
        true
    | TypeVariable(_), _                                        ->
        (union n m table);
        true
    | _, TypeVariable(_)                                        ->
        (union m n table);
        true
    | _, _                                                      ->
        false

let init t table =
  let rec dfs t =
    match t with
      BasicType(_)        -> ()
    | UnaryOp(str1, s')   ->
        Hashtbl.add table t ((Hashtbl.length table), t);
        (dfs s')
    | BinOp(str1, s1, s2) ->
        Hashtbl.add table t ((Hashtbl.length table), t);
        (dfs s1);
        (dfs s2);
    | TypeVariable(_)     ->
        Hashtbl.add table t ((Hashtbl.length table), t)
  in
  dfs t

let print_table table =
  Hashtbl.iter
  (
    fun a (set_num, t) ->
      print_endline ((string_of_type_expr a) ^ " --> " ^ (string_of_int set_num) ^ ", " ^ (string_of_type_expr t))
  ) table

let rec infer_expr tenv ttable = function
  | Id(vname) -> Type.lookup vname tenv
  | IntConst(_) -> BasicType("int")
  | BoolConst(_) -> BasicType("boolean")
  | Not(e) ->
      let te = infer_expr tenv ttable e in
      if (unify (BasicType("boolean")) te ttable) then
        BasicType("boolean")
      else
        raise (TypeError ((string_of_expr e) ^ " couldn't unify with boolean."))
  | Add(e1, e2) as e ->
      let t1 = infer_expr tenv ttable e1
      and t2 = infer_expr tenv ttable e2 in
      if (unify t1 (BasicType("int")) ttable) && (unify t2 (BasicType("int")) ttable) && (unify t1 t2 ttable) then
        BasicType("int")
      else
        raise (TypeError ((string_of_expr e ^ " couldn't unify with int.")))
  | And(e1, e2) as e ->
      let t1 = infer_expr tenv ttable e1
      and t2 = infer_expr tenv ttable e2 in
      if (unify t1 (BasicType("boolean")) ttable) && (unify t2 (BasicType("boolean")) ttable) && (unify t1 t2 ttable) then
        BasicType("boolean")
      else
        raise (TypeError ((string_of_expr e ^ " couldn't unify with boolean.")))
 | _ -> raise (TypeError "Sorry! Not implemented!")
(*
  
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | Equals    of expr * expr
  | FunApp    of expr * expr
  | Lambda    of string * expr

*)

let infer_fundef tenv arg fdef fundef =
    BinOp("fun", BasicType("int"), BasicType("int"))

let infer tenv ttable = function
    VarDef(vname, e) ->
      let vtype = infer_expr tenv ttable e in
      (Type.add_binding vname vtype tenv)
  | FunDef(fname, arg, e) ->
      let ftype = infer_fundef tenv ttable arg e in
      Type.add_binding fname ftype tenv

(*
  Treats the program as a list of definitions. Just iterates through each, and
  adding the type binding to the type environment for the further type-checking.
*)
let typecheck deflist ttable =
  let rec iter tenv = function
    [] -> tenv
  | def :: other_defs ->
      let new_tenv = infer tenv ttable def in
      iter new_tenv other_defs
  in
  iter (Type.empty_env ()) deflist

let t1 () =
  let a2 = TypeVariable("a2") in
  let a3 = TypeVariable("a3") in
  let s =
    BinOp(
      "fun",
      BinOp(
        "mul",
        BinOp(
          "fun",
          TypeVariable("a1"),
          a2
        ),
        UnaryOp(
          "list",
          a3
        )
      ),
      UnaryOp(
        "list",
        a2
      )
    )
  and t =
    BinOp(
      "fun",
      BinOp(
        "mul",
        BinOp(
          "fun",
          a3,
          TypeVariable("a4")
        ),
        UnaryOp(
          "list",
          a3
        )
      ),
      TypeVariable("a5")
    )
  in
  let table = Hashtbl.create 30 in
    print_endline ("s = " ^ (string_of_type_expr s));
    print_endline ("t = " ^ (string_of_type_expr t));
    init s table;
    init t table;
    print_table table;
    let result = unify s t table in
    Printf.printf "result = %b\n" result;
    print_table table

let t2 () =
  let a2 = TypeVariable("a2") in
  let a3 = TypeVariable("a3") in
  let s =
    BinOp(
      "fun",
      BinOp(
        "mul",
        BinOp(
          "fun",
          TypeVariable("a1"),
          a2
        ),
        UnaryOp(
          "list",
          a3
        )
      ),
      UnaryOp(
        "list",
        a2
      )
    )
 in
  let table = Hashtbl.create 30 in
    print_endline ("s = " ^ (string_of_type_expr s));
    init s table;
    print_table table;
    let pte = ForAll("a2", ForAll("a3", TypeExpr(s))) in
    let te = instantiate_polymorphic_type_expr pte table in
    print_table table;
    print_endline (string_of_type_expr te)

let _ = t1 ()
