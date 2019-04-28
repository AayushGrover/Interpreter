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
let rec eval e env : Expression.exprResult =
  match e with
  | Expression.MapConst(m)        -> {Expression.res_expr = e ; Expression.res_env = env}
  | Expression.StackConst(ml)     -> {Expression.res_expr = e ; Expression.res_env = env}
  | Expression.Id(vname)          -> {Expression.res_expr = (Expression.apply vname env) ; Expression.res_env = env} 
  | Expression.IntConst(_)        -> {Expression.res_expr = e ; Expression.res_env = env}
  | Expression.BoolConst(_)       -> {Expression.res_expr = e ; Expression.res_env = env}
  | Expression.Closure(_, _, _)   -> {Expression.res_expr = e ; Expression.res_env = env}
  | Expression.Add(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let i1 = (getIntConstValue (e1').Expression.res_expr) and i2 = (getIntConstValue (e2').Expression.res_expr)
        in
        {Expression.res_expr = (Expression.IntConst(i1 + i2)) ; Expression.res_env = env}
  | Expression.Sub(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
      let i1 = (getIntConstValue (e1').Expression.res_expr) and i2 = (getIntConstValue (e2').Expression.res_expr)
        in
        {Expression.res_expr = (Expression.IntConst(i1 - i2)) ; Expression.res_env = env}
  | Expression.If(b, e1, e2)      -> if (getBoolConstValue (eval b env).Expression.res_expr) then (eval e1 env) else (eval e2 env)
  | Expression.Let(vname, e1, e2) ->
      let env' = (Expression.addBinding vname (eval e1 env).Expression.res_expr env)
      in
      {Expression.res_expr = (eval e2 env').Expression.res_expr ; Expression.res_env = env}
  | Expression.Not(e)             ->
      let e' = (eval e env)
      in
        let b' = (getBoolConstValue (e').Expression.res_expr)
        in
        {Expression.res_expr = Expression.BoolConst(not b') ; Expression.res_env = env}
  | Expression.Or(e1, e2)         ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue (e1').Expression.res_expr) and b2' = (getBoolConstValue (e2').Expression.res_expr)
        in
        {Expression.res_expr = Expression.BoolConst(b1' || b2') ; Expression.res_env = env}
  | Expression.And(e1, e2)        ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
        let b1' = (getBoolConstValue (e1').Expression.res_expr) and b2' = (getBoolConstValue (e2').Expression.res_expr)
        in
        {Expression.res_expr = Expression.BoolConst(b1' && b2') ; Expression.res_env = env}
  | Expression.Equals(e1, e2)     ->
      let e1' = (eval e1 env) and e2' = (eval e2 env)
      in
      (
        match((e1').Expression.res_expr, (e2').Expression.res_expr) with
          (Expression.BoolConst(_), Expression.BoolConst(_)) ->
            let b1' = (getBoolConstValue (e1').Expression.res_expr) and b2' = (getBoolConstValue (e2').Expression.res_expr)
            in
            {Expression.res_expr = Expression.BoolConst(b1' = b2') ; Expression.res_env = env}
        | (Expression.IntConst(_), Expression.IntConst(_))   ->
            let i1' = (getIntConstValue (e1').Expression.res_expr) and i2' = (getIntConstValue (e2').Expression.res_expr)
            in
            {Expression.res_expr = Expression.BoolConst(i1' = i2') ; Expression.res_env = env}
        | _                            ->
            raise (TypeError "evaluate.Equals: both e1 and e2 should evaluate to expressions of the same type")
      )
  | Expression.FunDef(par, body) -> {Expression.res_expr = Expression.Closure(par, body, env) ; Expression.res_env = env}
    (* The only thing to evaluate on seeing a function definition is the closure. *)
  | Expression.FunApp(f, arg) ->
    (* The function application evaluates the function f and the argument arg in turn and
       applies f on arg. This is done by creating a new environment e'' by binding the parameter par
       to the arg' (the value to which arg evaluates) and adding th.Expression.res_expr.Expression.res_expr.Expression.res_expre same to env', which is the 
       environment in the closure obtained by evaluating f in env. *)
      let e' = (eval f env)
      in
        let (par, body, env') = (getClosureValue (e').Expression.res_expr) and arg' = (eval arg env)
        in
          let env'' = (Expression.addBinding par (arg').Expression.res_expr env')
          in
          (eval body env'')
  | Expression.CreateStack(id) -> 
      {Expression.res_expr = e ; Expression.res_env = (Expression.addBinding id (Expression.StackConst Expression.EmptyStack) env)}
  | Expression.PushToStack(id,v) -> 
      {Expression.res_expr = e ; Expression.res_env = (Expression.addElementToStack v id env)}
  | Expression.TopOfStack(id) ->
      {Expression.res_expr = (Expression.topOfStack id env) ; Expression.res_env = env}
  | Expression.PopFromStack(id) ->
      {Expression.res_expr = e ; Expression.res_env = (Expression.popFromStack id env)}
  | Expression.CreateMap(id) -> 
      {Expression.res_expr = e ; Expression.res_env = (Expression.addBinding id (Expression.MapConst (Expression.Map(Hashtbl.create 500))) env)}
  | Expression.AddMapping(id, key, value) ->
      {Expression.res_expr = e; Expression.res_env = (Expression.addMapping key value id env)}
  | Expression.GetMapValue(id, key) ->
      {Expression.res_expr = (Expression.getValueFromKey key id env) ; Expression.res_env = env}
  | Expression.EndProgram -> 
      {Expression.res_expr = e ; Expression.res_env = env}
