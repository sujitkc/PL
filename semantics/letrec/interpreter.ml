exception TypeError of string

(* Interpreter *)
let rec eval e env : Expression.expr =
  match e with
  | Expression.Id(vname)            -> Expression.apply vname env
  | Expression.IntConst(_)          -> e
  | Expression.BoolConst(_)         -> e
  | Expression.Closure(_, _, _)
  | Expression.RecClosure(_, _, _) -> e
  | Expression.Add(e1, e2)          ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      let i1 = (Expression.getIntConstValue e1') and i2 = (Expression.getIntConstValue e2') in
        Expression.IntConst(i1 + i2)
  | Expression.Sub(e1, e2)          ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      let i1 = (Expression.getIntConstValue e1') and i2 = (Expression.getIntConstValue e2') in
        Expression.IntConst(i1 - i2)
  | Expression.Multiply(e1, e2)     ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      let i1 = (Expression.getIntConstValue e1') and i2 = (Expression.getIntConstValue e2') in
        Expression.IntConst(i1 * i2)
  | Expression.If(b, e1, e2)        -> if (Expression.getBoolConstValue (eval b env)) then (eval e1 env) else (eval e2 env)
  | Expression.Let(vname, e1, e2)   ->
      let env' = (Expression.addBinding vname (eval e1 env) env) in
      (eval e2 env')
  | Expression.Not(e)               ->
      let e' = (eval e env) in
      let b' = (Expression.getBoolConstValue e') in
        Expression.BoolConst(not b')
  | Expression.Or(e1, e2)           ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      let b1' = (Expression.getBoolConstValue e1') and b2' = (Expression.getBoolConstValue e2') in
        Expression.BoolConst(b1' || b2')
  | Expression.And(e1, e2)          ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      let b1' = (Expression.getBoolConstValue e1') and b2' = (Expression.getBoolConstValue e2') in
        Expression.BoolConst(b1' && b2')
  | Expression.Equals(e1, e2)       ->
      let e1' = (eval e1 env) and e2' = (eval e2 env) in
      (
        match(e1', e2') with
          (Expression.BoolConst(_), Expression.BoolConst(_)) ->
            let b1' = (Expression.getBoolConstValue e1') and b2' = (Expression.getBoolConstValue e2')
            in
            Expression.BoolConst(b1' = b2')
        | (Expression.IntConst(_), Expression.IntConst(_))   ->
            let i1' = (Expression.getIntConstValue e1') and i2' = (Expression.getIntConstValue e2')
            in
            Expression.BoolConst(i1' = i2')
        | _                            ->
            raise (TypeError "evaluate.Equals: both e1 and e2 should evaluate to expressions of the same type")
      )
  | Expression.FunDef(par, body)   -> Expression.Closure(par, body, env)
    (* The only thing to evaluate on seeing a function definition is the closure. *)
  | Expression.FunApp(f, arg)      ->
    (* The function application evaluates the function f and the argument arg in turn and
       applies f on arg. This is done by creating a new environment e'' by binding the parameter par
       to the arg' (the value to which arg evaluates) and adding the same to env', which is the 
       environment in the closure obtained by evaluating f in env. *)
      let e' = (eval f env) in
      let (par, body, env') = (Expression.getClosureValue e') and arg' = (eval arg env) in
      let env'' = (Expression.addBinding par arg' env') in
        (eval body env'')
  | Expression.RecFunDef(par, body)           -> Expression.RecClosure(par, body, env)  
