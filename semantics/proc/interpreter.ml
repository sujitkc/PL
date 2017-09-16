exception TypeError of string

let getIntConstValue e =
  match e with
    Expression.IntConst(c) -> c
  | _        -> raise (TypeError (("getIntConstValue: The expression is not in IntConst normal form.") ^ (Expression.string_of_expr e)))
 
let getBoolConstValue e =
  match e with
    Expression.BoolConst(b) -> b
  | _            -> raise (TypeError (("getBoolConstValue: The expression is not in BoolConst normal form.") ^ (Expression.string_of_expr e)))

(* This is the new addition to our extractor functions. The only difference here is that it returns a tuple,
   as the normal form Closures 3 and not 1 underlying value. *)
let getClosureValue e =
  match e with
    Expression.Closure(par, body, env)    -> (par, body, env)
  | _            -> raise (TypeError "getFunDefValue: The expression is not in FunDef normal form.")

(* Interpreter *)
let rec eval e env : Expression.expr =
  match e with
  | Expression.Id(vname)          -> Expression.apply vname env
  | Expression.IntConst(_)        -> e
  | Expression.BoolConst(_)       -> e
  | Expression.Closure(_, _, _)   -> e
  | Expression.Add(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        Expression.IntConst(i1 + i2)
  | Expression.Sub(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue e1') and i2 = (getIntConstValue e2')
        in
        Expression.IntConst(i1 - i2)
  | Expression.If(b, e1, e2)      -> if (getBoolConstValue (eval b env)) then (eval e1 env) else (eval e2 env)
  | Expression.Let(vname, e1, e2) ->
      let env' = (Expression.addBinding vname (eval e1 env) env)
      in
      (eval e2 env')
  | Expression.Not(e)             ->
      let e' = (eval e env)
      in
        let b' = (getBoolConstValue e')
        in
        Expression.BoolConst(not b')
  | Expression.Or(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        Expression.BoolConst(b1' || b2')
  | Expression.And(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
        in
        Expression.BoolConst(b1' && b2')
  | Expression.Equals(e1, e2)     ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
      (
        match(e1', e2') with
          (Expression.BoolConst(_), Expression.BoolConst(_)) ->
            let b1' = (getBoolConstValue e1') and b2' = (getBoolConstValue e2')
            in
            Expression.BoolConst(b1' = b2')
        | (Expression.IntConst(_), Expression.IntConst(_))   ->
            let i1' = (getIntConstValue e1') and i2' = (getIntConstValue e2')
            in
            Expression.BoolConst(i1' = i2')
        | _                            ->
            raise (TypeError "evaluate.Equals: both e1 and e2 should evaluate to expressions of the same type")
      )
  | Expression.FunDef(par, body) -> Expression.Closure(par, body, env)
    (* The only thing to evaluate on seeing a function definition is the closure. *)
  | Expression.FunApp(f, arg) ->
    (* The function application evaluates the function f and the argument arg in turn and
       applies f on arg. This is done by creating a new environment e'' by binding the parameter par
       to the arg' (the value to which arg evaluates) and adding the same to env', which is the 
       environment in the closure obtained by evaluating f in env. *)
      let e' = (eval f env)
      in
        let (par, body, env') = (getClosureValue e') and arg' = (eval arg env)
        in
          let env'' = (Expression.addBinding par arg' env')
          in
          (eval body env'')
