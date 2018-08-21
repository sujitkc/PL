exception TypeError of string

let rec string_of_expr = function
  | Expression.Id(vname)           -> vname
  | Expression.IntConst(n)         -> string_of_int n
  | Expression.Add(e1, e2)         -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Expression.Sub(e1, e2)         -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | Expression.If(b, e1, e2)       -> "if (" ^ (string_of_expr b) ^ ") then (" ^ (string_of_expr e1) ^ ") else (" ^ (string_of_expr e2) ^ ")"
  | Expression.Let(vname, e1, e2)  -> "let " ^ vname ^ " = (" ^ (string_of_expr e1) ^ ") in (" ^ (string_of_expr e2) ^ ")"
  | Expression.BoolConst(b)        -> string_of_bool b
  | Expression.Not(e)              -> "not(" ^ string_of_expr e ^ ")"
  | Expression.Or(e1, e2)          -> "(" ^ (string_of_expr e1) ^ ") or (" ^ (string_of_expr e2) ^ ")"
  | Expression.And(e1, e2)         -> "(" ^ (string_of_expr e1) ^ ") and (" ^ (string_of_expr e2) ^ ")"
  | Expression.Equals(e1, e2)      -> "(" ^ (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"

(* Interpreter *)
let rec eval e env : Expression.type_let2 =
  match e with
  | Expression.Id(vname)          -> Env.apply vname env
  | Expression.IntConst(_)        -> Expression.Integer
  | Expression.BoolConst(_)       -> Expression.Boolean
  | Expression.Add(e1, e2)
  | Expression.Sub(e1, e2)        ->
    (
      match ((eval e1 env), (eval e2 env)) with
        Expression.Integer, Expression.Integer -> Expression.Integer
      | _                                           -> raise (TypeError("Type error in " ^ (string_of_expr e))) 
    )
  | Expression.If(b, e1, e2)      ->
    (
      match ((eval b env), (eval e1 env), (eval e2 env)) with
        Expression.Boolean, Expression.Integer, Expression.Integer -> Expression.Integer
      | Expression.Boolean, Expression.Boolean, Expression.Boolean -> Expression.Boolean
      | _                                                           -> raise (TypeError("Type error in " ^ (string_of_expr e)))
    )
  | Expression.Let(vname, e1, e2) ->
      let env' = (Env.addBinding vname (eval e1 env) env) in
      (eval e2 env')
  | Expression.Not(e)             ->
      if (eval e env) = Expression.Boolean then Expression.Boolean
      else raise (TypeError("Type error in " ^ (string_of_expr e)))
  | Expression.Or(e1, e2)
  | Expression.And(e1, e2)        ->
    ( 
      match ((eval e1 env), (eval e2 env)) with
        Expression.Boolean, Expression.Boolean -> Expression.Boolean
      | _                                      -> raise (TypeError("Type error in " ^ (string_of_expr e)))
    )
  | Expression.Equals(e1, e2)     ->
      if (eval e1 env) = (eval e2 env) then Expression.Boolean
      else raise (TypeError("Type error in " ^ (string_of_expr e)))
