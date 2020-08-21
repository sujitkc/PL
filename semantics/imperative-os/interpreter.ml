exception TypeError of string

let string_of_typ = function
  | Expression.Boolean -> "boolean"
  | Expression.Integer -> "int"

let init_value = function
  | Expression.Boolean -> Expression.BoolConst(true)
  | Expression.Integer -> Expression.IntConst(0)

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
  | Expression.LT(e1, e2)        -> 
      "(" ^ (string_of_expr e1) ^ ") < (" ^
      (string_of_expr e2) ^ ")"
  | Expression.Equals(e1, e2)     -> "(" ^
      (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"
  | Expression.FunCall(_, _)      -> "function call"
  | Expression.Closure(_, _, _, _) -> "closure"

let rec string_of_stmt = function
    | Expression.Assignment(s, e) ->
        s ^ " := " ^ (string_of_expr e) ^ ";"
    | Expression.Return(e) ->
        "return " ^ (string_of_expr e) ^ ";"
    | Expression.Block(decls, slist) -> "{\n" ^
        (List.fold_left (fun x y -> x ^
        (string_of_stmt y) ^ "\n") "" slist) ^ "}"
    | Expression.While(c, s) ->
      "while(" ^ (string_of_expr c) ^ ") " ^ (string_of_stmt s)

let string_of_decl (vname, ty) =
  (string_of_typ ty) ^ " " ^ vname

let string_of_fundef fdef =
  string_of_typ fdef.Expression.rtype ^ " " ^
  fdef.Expression.fname ^ "(" ^
  (List.fold_left
    (fun x y -> x ^ (string_of_decl y) ^ ", ")
    "" 
    fdef.Expression.params) ^ ") " ^
      (string_of_stmt fdef.Expression.body) ^ "\n"
  
let string_of_program p =
  (List.fold_left
    (fun x y -> x ^ (string_of_decl y) ^ ";\n")
     "" p.Expression.decls)
  ^
  (List.fold_left
    (fun x y -> x ^ (string_of_fundef y) ^ "\n")
     "" p.Expression.fundefs)
  ^
  (string_of_stmt p.Expression.statement)


let get_int_const_value e =
  match e with
    Expression.IntConst(c) -> c
  | _ -> raise (TypeError (("get_int_const_value: The expression is \
               not in IntConst normal form.") ^ (string_of_expr e)))
 
let get_bool_const_value e =
  match e with
    Expression.BoolConst(b) -> b
  | _ -> raise (TypeError (("get_bool_const_value: The expression is \
               not in BoolConst normal form.") ^ (string_of_expr e)))

let get_closure_value e =
  match e with
    Expression.Closure(par, body, env, fenv)    -> (par, body, env, fenv)
  | _ -> raise (TypeError "getFunDefValue: The expression is not in \
               FunDef normal form.")

(* Evaluate declarations *)
let evaluate_declarations decs env =
  let rec iter decs env =
    match decs with
      [] -> env
    | (vname, typ) :: tl ->
        iter tl (Environment.add_binding vname (init_value typ) env)
  in
  iter decs env
    
(* Evaluate - Expression *)
let rec evaluate_expr e env fenv =
  match e with
  | Expression.Id(vname)          -> (Some(Environment.apply vname env), env)
  | Expression.IntConst(_)        -> (Some(e), env)
  | Expression.BoolConst(_)       -> (Some(e), env)
  | Expression.Closure(_,_,_,_)   -> (Some(e), env)
  | Expression.Add(e1, e2) ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      (
        match t1, t2 with
          Some(Expression.IntConst(i1)), Some(Expression.IntConst(i2)) ->
            (Some(Expression.IntConst(i1 + i2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
      )
  | Expression.Sub(e1, e2)        ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      (
        match t1, t2 with
          Some(Expression.IntConst(i1)), Some(Expression.IntConst(i2)) ->
           (Some(Expression.IntConst(i1 - i2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
      )
  | Expression.If(b, e1, e2)      ->
    (
      let (bval, env1)  = evaluate_expr b env fenv in
        match bval with
          Some(Expression.BoolConst(true))  -> evaluate_expr e1 env1 fenv
        | Some(Expression.BoolConst(false)) -> evaluate_expr e2 env1 fenv
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
    )
  | Expression.Let(vname, e1, e2) ->
      let (e1_opt, env1) = evaluate_expr e1 env fenv in
      begin
        match e1_opt with
          Some(e1') ->
            let env' = (Environment.add_binding vname e1' env1) in
            (evaluate_expr e2 env' fenv)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
      end
  | Expression.Not(e') ->
      let (v, env') = evaluate_expr e' env fenv in
      begin
        match v with
          Some(Expression.BoolConst(b)) -> (Some(Expression.BoolConst(not(b))), env')
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
      end
  | Expression.Or(e1, e2) ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      begin
        match t1, t2 with
          (Some(Expression.BoolConst(b1)), Some(Expression.BoolConst(b2))) ->
            (Some(Expression.BoolConst(b1 || b2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
      end
  | Expression.And(e1, e2) ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      begin
        match (t1, t2) with
          (Some(Expression.BoolConst(b1)), Some(Expression.BoolConst(b2))) ->
            (Some(Expression.BoolConst(b1 && b2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e)))
      end
  | Expression.LT(e1, e2) ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      begin
        match (t1, t2) with
          (Some(Expression.IntConst(i1)), Some(Expression.IntConst(i2))) ->
            (Some(Expression.BoolConst(i1 < i2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
      end

  | Expression.Equals(e1, e2) ->
      let (t1, env1) = evaluate_expr e1 env fenv in
      let (t2, env2) = evaluate_expr e2 env1 fenv in
      begin
        match (t1, t2) with
          (Some(Expression.IntConst(i1)), Some(Expression.IntConst(i2))) ->
            (Some(Expression.BoolConst(i1 = i2)), env2)
        | (Some(Expression.BoolConst(b1)), Some(Expression.BoolConst(b2))) ->
            (Some(Expression.BoolConst(b1 = b2)), env2)
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
      end
  | Expression.FunCall(fname, args)      ->
      let c = Environment.apply fname fenv in
      begin
        match c with
          Expression.Closure(parlist, body, env', fenv') -> 
      (*
        For evaluating the body, we have to evaluate the arguments. On evaluating each
        argument, it will give an argument value and an updated value environment (the 
        function environment will remain unchanged). The values to be supplied to the
        evaluation of the function body are the list of argument values (avals) and the final
        value environment computed after evaluating the nth argument (env_n).  
      *)
          let (avals, env_n) =
            let rec eval_arglist args env' avals =
              match args with
                [] -> ((List.rev avals), env)
              | arg :: arglist' ->
                  begin 
                    match evaluate_expr arg env' fenv with 
                      (Some(aval), env'') ->
                        eval_arglist arglist' env'' (aval::avals)
                    | _ ->  raise (TypeError("Type error in " ^ (string_of_expr e)))
                  end
            in
            eval_arglist args env []
          in
      (*
        The environment in which the body is to be evaluated is the environment contained
        in the closure, augmented with the parameters of the closure mapped to the 
        respective argument values computed above. augment_env function does exactly this.
        The function environment to be supplied
        to this evaluation is that contained in the closure (fenv').
      *)
          let body_env =
            let rec augment_env plist avals env =
              match (plist, avals) with
                ([], []) -> env
              | (p :: plist', av :: avs') ->
                  augment_env plist' avs' (Environment.add_binding p av env)
              | _ -> failwith "Mismatch in the length of parameter list and argument list."
            in
            augment_env parlist avals env'
          in
          evaluate_stmt body body_env fenv'
        | _ -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
      end

and evaluate_stmt stmt env fenv =
  match stmt with
  | Expression.Assignment(vname, e) ->
      let (v, env') = evaluate_expr e env fenv in
      begin
        match v with
          Some(e) ->
            let env'' = Environment.update_binding vname e env' in
            (None, env'')
        | _ ->  raise (TypeError("Type error in " ^ (string_of_stmt stmt)))
      end
  | Expression.Return(e) -> evaluate_expr e env fenv
  | Expression.Block(decls, stmt_list) -> evaluate_block stmt env fenv
  | Expression.While(c, s) ->
      let tf, env' = evaluate_expr c env fenv in
      if tf = Some(Expression.BoolConst(true)) then
        let v, env'' = evaluate_stmt s env fenv in
        match v with
          None -> evaluate_stmt stmt env'' fenv
        | Some(_) -> (v, env'')
      else if tf = Some(Expression.BoolConst(false)) then
        (None, env')
      else
         raise (TypeError("Type error in condition of while loop." ^ (string_of_stmt stmt))) 

and evaluate_block block env fenv =
  match block with
    Expression.Block(decls, stmt_list) ->
      let env' = Environment.enter_scope env in
      let env'' = evaluate_declarations decls env' in
      let (v, env''') = evaluate_stmtlist stmt_list env'' fenv in
      let env'''' = Environment.exit_scope env''' in
      (v, env'''')
 | _ -> failwith "evaluate_block: statement type not supported." 

and evaluate_stmtlist stmtlist env fenv =
  match stmtlist with
    [] -> (None, env)
  | stmt :: tl ->
    begin
      let (v, env') = evaluate_stmt stmt env fenv in
        match v with
          None -> evaluate_stmtlist tl env' fenv
        | Some(_) -> (v, env')
    end

(* evaluate function definition *)
let evaluate_fundef fd env fenv =
  let fname, params, body = fd.Expression.fname, fd.Expression.params, fd.Expression.body  in
  let pars = List.map (fun (a, _) -> a) params in
  let closure = Expression.Closure(pars, body, env, fenv) in
  Environment.add_binding fname closure fenv

let evaluate_fundefs fdefs env =
  let rec iter fdefs fenv =
    match fdefs with
      [] -> fenv
    | fdef :: fdefs' ->
        try
          (* if a function of the same name has already
              been defined then raise an exception. *)
          let _ = Environment.apply fdef.Expression.fname fenv in
          raise (TypeError("Duplicate function definition " ^
                            fdef.Expression.fname))
        with
          Not_found ->
            let fenv' = evaluate_fundef fdef env fenv in
            iter fdefs' fenv'
  in
  iter fdefs Environment.EmptyEnv
   
(* Evaluator - Program *)
let evaluate_prog p =
  let env = Environment.enter_scope Environment.EmptyEnv in
  let env' = evaluate_declarations p.Expression.decls env in
  let fenv = evaluate_fundefs p.Expression.fundefs env' in
    evaluate_stmt p.Expression.statement env' fenv
