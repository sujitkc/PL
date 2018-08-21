exception TypeError of string

let string_of_typ = function
  | Expression.Boolean -> "boolean"
  | Expression.Integer -> "int"

let rec string_of_expr = function
  | Expression.Id(vname)          -> vname
  | Expression.IntConst(n)        -> string_of_int n
  | Expression.Add(e1, e2)        ->
      (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Expression.Sub(e1, e2)        ->
      (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | Expression.If(b, e1, e2)      ->
      "if (" ^(string_of_expr b) ^ ") then (" ^
      (string_of_expr e1) ^ ") else (" ^
      (string_of_expr e2) ^ ")"
  | Expression.Let(vname, e1, e2) ->
      "let " ^ vname ^ " = (" ^ (string_of_expr e1) ^
      ") in (" ^ (string_of_expr e2) ^ ")"
  | Expression.BoolConst(b)       -> string_of_bool b
  | Expression.Not(e)             ->
      "not(" ^ string_of_expr e ^ ")"
  | Expression.Or(e1, e2)         ->
      "(" ^ (string_of_expr e1) ^ ") or (" ^ 
      (string_of_expr e2) ^ ")"
  | Expression.And(e1, e2)        -> 
      "(" ^ (string_of_expr e1) ^ ") and (" ^
      (string_of_expr e2) ^ ")"
  | Expression.Equals(e1, e2)     -> "(" ^
      (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"
  | Expression.FunCall(_, _)      -> "function call"

let rec string_of_stmt = function
    | Expression.Program.Assignment(s, e) ->
        s ^ " := " ^ (string_of_expr e) ^ ";"
    | Expression.Program.Return(e) ->
        "return " ^ (string_of_expr e) ^ ";"
    | Expression.Program.Block(slist) -> "{\n" ^
        (List.fold_left (fun x y -> x ^
        (string_of_stmt y) ^ "\n") "" slist) ^ "}"

let string_of_decl (vname, ty) =
  (string_of_typ ty) ^ " " ^ vname

let string_of_fundef fdef =
  string_of_typ fdef.Expression.Program.rtype ^ " " ^
  fdef.Expression.Program.fname ^ "(" ^
  (List.fold_left
    (fun x y -> x ^ (string_of_decl y) ^ ", ")
    "" 
    fdef.Expression.Program.params) ^ ") " ^
      (string_of_stmt fdef.Expression.Program.body) ^ "\n"
  
let string_of_program p =
  (List.fold_left
    (fun x y -> x ^ (string_of_decl y) ^ ";\n")
     "" p.Expression.Program.decls)
  ^
  (List.fold_left
    (fun x y -> x ^ (string_of_fundef y) ^ "\n")
     "" p.Expression.Program.fundefs)

(* Typecheck declarations *)
let typecheck_declarations decs =
  let rec iter decs env =
    match decs with
      [] -> env
    | (vname, typ) :: tl ->
        iter tl (Env.addBinding vname typ env)
  in
  iter decs Env.EmptyEnv
    
(* Typechecker - Expression *)
let rec typecheck_expr e env fenv : Expression.typ =
  match e with
  | Expression.Id(vname)          -> Env.apply vname env
  | Expression.IntConst(_)        -> Expression.Integer
  | Expression.BoolConst(_)       -> Expression.Boolean
  | Expression.Add(e1, e2)
  | Expression.Sub(e1, e2)        ->
      let t1 = typecheck_expr e1 env fenv
      and t2 = typecheck_expr e2 env fenv in
      (
        match t1, t2 with
          Expression.Integer, Expression.Integer -> Expression.Integer
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
      )
  | Expression.If(b, e1, e2)      ->
    (
      let btype  = typecheck_expr b env fenv
      and e1type = typecheck_expr e1 env fenv
      and e2type = typecheck_expr e2 env fenv in
      match btype, e1type, e2type with
        Expression.Boolean,
        Expression.Integer,
        Expression.Integer -> Expression.Integer
      | Expression.Boolean,
        Expression.Boolean,
        Expression.Boolean -> Expression.Boolean
      | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
    )
  | Expression.Let(vname, e1, e2) ->
      let e1type = typecheck_expr e1 env fenv in
      let env' = (Env.addBinding vname e1type env) in
      (typecheck_expr e2 env' fenv)
  | Expression.Not(e) ->
      let etype = typecheck_expr e env fenv in
      if etype = Expression.Boolean then
        Expression.Boolean
      else
        raise (TypeError("Type error in " ^ (string_of_expr e)))
  | Expression.Or(e1, e2)
  | Expression.And(e1, e2)
  | Expression.Equals(e1, e2) ->
      let e1type = typecheck_expr e1 env fenv
      and e2type = typecheck_expr e2 env fenv in
      if e1type = e2type then Expression.Boolean
      else raise (TypeError("Type error in " ^ (string_of_expr e)))
  | Expression.FunCall(fname, args)      ->
      let paramtypes, rtype = Env.apply fname fenv
      and argtypes = List.map
                    (
                      fun arg -> typecheck_expr arg env fenv
                    ) args in
      (* check if the number of arguments matches the number of parameters *)
      if (List.length paramtypes) <> (List.length argtypes) then
        raise (TypeError("Type error in function call " ^
                         fname ^
                         " : number of arguments don't match."))
      else
        (* check if the argument types match the parameter types *)
        let rec iter pts ats = (* param-types and arg-types *)
          match (pts, ats) with
            [], [] -> ()
          | pt :: pts', at :: ats' ->
              if pt = at then iter pts' ats'
              else
                raise (TypeError("Type error in function call " ^
                                 fname ^
                                 " : argument type mismatch."))
          | _, _ -> 
              raise (TypeError("Type error in function call " ^
                               fname ^
                               " : something else wrong in arg-list."))
        in
        let _ = iter argtypes paramtypes in
          rtype

let rec typecheck_stmt stmt env fenv =
  match stmt with
  | Expression.Program.Assignment(vname, e) ->
      let vtype = Env.apply vname env
      and etype = typecheck_expr e env fenv in
      if vtype = etype then
        None
      else
        raise (TypeError("Type error in statement with " ^ (string_of_expr e))) 
  | Expression.Program.Return(e) -> Some(typecheck_expr e env fenv)
  | Expression.Program.Block(stmt_list) ->
      typecheck_stmtblock stmt_list env fenv

and typecheck_stmtblock block env fenv =
  match block with
    [] -> None
  | stmt :: tl ->
    (
      let stype = typecheck_stmt stmt env fenv in
        match stype with
          None -> typecheck_stmtblock tl env fenv
        | Some(_) -> stype
    )

(* typecheck function definition *)
let typecheck_fundef
    (fd : Expression.Program.fundef) env fenv :
    Expression.typ list * Expression.typ =
  let btype = typecheck_stmt fd.Expression.Program.body env fenv in
  match btype with
    None -> raise (TypeError("Type error in function body " ^
                            fd.Expression.Program.fname))
  | Some(_) ->
      let paramtypes = List.map (
        fun (x, y) -> y
      ) fd.Expression.Program.params in
      (paramtypes, fd.Expression.Program.rtype)

let typecheck_fundefs (fdefs : Expression.Program.fundef list)
    (env : (string , Expression.typ) Env.env) :
    (string, (Expression.typ list * Expression.typ)) Env.env =
  let rec iter fdefs env fenv =
    match fdefs with
      [] -> fenv
    | fdef :: tl ->
        try
          (* if a function of the same name has already
              been defined then raise an exception. *)
          let _ = Env.apply fdef.Expression.Program.fname fenv in
          raise (TypeError("Duplicate function definition " ^
                            fdef.Expression.Program.fname))
        with
          Not_found ->
            let fdef_typ = typecheck_fundef fdef env fenv in
            iter tl env 
              ( Env.addBinding
                  fdef.Expression.Program.fname fdef_typ fenv)
  in
  iter fdefs env Env.EmptyEnv
   
(* Typechecker - Program *)
let typecheck_prog (p : Expression.Program.program) : unit =
  let env = typecheck_declarations p.Expression.Program.decls in
  let _ = typecheck_fundefs p.Expression.Program.fundefs env in
    ()
