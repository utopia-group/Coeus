open Core
open Ast.Coeus

let op_one op e =
  Expr.BinaryExpr (op, e, Expr.LiteralExpr (Literal.IntLit Bigint.one))

let plus_one = op_one BinaryOperator.Plus

let minus_one = op_one BinaryOperator.Minus

let get_step_expr counter direction update_expr =
  let dispatch_op op e =
    match (direction, op) with
    | Stmt.ForDirection.Forward, BinaryOperator.Plus
     |Stmt.ForDirection.Backward, BinaryOperator.Minus ->
        Some e
    | _, _ -> None
  in
  match Expr.simplify update_expr with
  | Expr.BinaryExpr (op, Expr.VarExpr v, rhs) when Identifier.equal v counter ->
      dispatch_op op rhs
  | Expr.BinaryExpr (op, lhs, Expr.VarExpr v) when Identifier.equal v counter ->
      dispatch_op op lhs
  | _ -> None

let convert_for counter lower cond body =
  let dispatch_body_check upper step direction body =
    let write_varset = Stmts.write_var_set_of body in
    if Hash_set.mem write_varset counter then None
    else
      let stmt = Stmt.For {counter; lower; upper; step; direction; body} in
      Some stmt
  in
  let dispatch_update upper direction =
    match List.split_n body (List.length body - 1) with
    | stmts, [Stmt.Assign {lhs= Lvalue.({base; _}); rhs}]
      when Identifier.equal base counter ->
        let open Option in
        get_step_expr base direction rhs
        >>= fun e -> dispatch_body_check upper e direction stmts
    | _, _ -> None
  in
  let dispatch_op vside op e =
    match (vside, op) with
    | Side.Left, BinaryOperator.Lt ->
        dispatch_update e Stmt.ForDirection.Forward
    | Side.Left, BinaryOperator.Le ->
        dispatch_update (plus_one e) Stmt.ForDirection.Forward
    | Side.Left, BinaryOperator.Gt ->
        dispatch_update e Stmt.ForDirection.Backward
    | Side.Left, BinaryOperator.Ge ->
        dispatch_update (minus_one e) Stmt.ForDirection.Backward
    | Side.Right, BinaryOperator.Lt ->
        dispatch_update e Stmt.ForDirection.Backward
    | Side.Right, BinaryOperator.Le ->
        dispatch_update (minus_one e) Stmt.ForDirection.Backward
    | Side.Right, BinaryOperator.Gt ->
        dispatch_update e Stmt.ForDirection.Forward
    | Side.Right, BinaryOperator.Ge ->
        dispatch_update (plus_one e) Stmt.ForDirection.Forward
    | _, _ -> None
  in
  match cond with
  | Expr.BinaryExpr (op, Expr.VarExpr v, rhs) when Identifier.equal v counter ->
      dispatch_op Side.Left op rhs
  | Expr.BinaryExpr (op, lhs, Expr.VarExpr v) when Identifier.equal v counter ->
      dispatch_op Side.Right op lhs
  | _ -> None

let rec run_stmts_impl acc = function
  | [] -> acc
  | [s] ->
      let s = run_stmt s in
      s :: acc
  | (Stmt.Assign {lhs= Lvalue.({base; indices= []}); rhs} as s0)
    :: Stmt.While {cond; body} :: rest
    -> (
      let body = run_stmts body in
      match convert_for base rhs cond body with
      | Some for_stmt ->
          let acc' = for_stmt :: acc in
          run_stmts_impl acc' rest
      | None ->
          let s1 = Stmt.While {cond; body} in
          let acc' = s1 :: s0 :: acc in
          run_stmts_impl acc' rest )
  | s0 :: s1 :: rest ->
      let s0 = run_stmt s0 in
      let acc' = s0 :: acc in
      run_stmts_impl acc' (s1 :: rest)

and run_stmt = function
  | Stmt.If {cond; then_branch; else_branch} ->
      Stmt.If
        { cond
        ; then_branch= run_stmts then_branch
        ; else_branch= run_stmts else_branch }
  | Stmt.While {cond; body} -> Stmt.While {cond; body= run_stmts body}
  | Stmt.For {counter; lower; upper; direction; step; body} ->
      Stmt.For {counter; lower; upper; direction; step; body= run_stmts body}
  | _ as s -> s

and run_stmts stmts =
  let rev_stmts = run_stmts_impl [] stmts in
  List.rev rev_stmts

let run_proc (proc: Procedure.t) =
  let stmts = run_stmts proc.stmts in
  {proc with stmts}

let run prog =
  Logs.debug (fun m -> m "ForStmtConverter starts") ;
  let prog' = proc_map prog ~f:run_proc in
  Logs.debug (fun m -> m "ForStmtConverter successfully finished") ;
  Result.Ok prog'
