open Core
open Ast.Ecoeus

let expr_one = Expr.mk_literal (Literal.IntLit Bigint.one)

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with
  | Stmt.({bare_stmt= For {step; _}; _}) :: _ when Expr.equal step expr_one ->
      true
  | _ -> false

let create_legitimacy_goal ast parent counter lower upper direction body =
  let subst_loop_counter body expr =
    Stmts.map_expr body
      ~f:
        (Expr.subst ~f:(fun id ->
             if Identifier.equal id counter then Some expr else None ))
  in
  let get_fresh_counter_expr name =
    let name = FreshNameGenerator.get_fresh_var ~name () in
    Expr.mk_var VarBinding.{name; ty= Type.IntType}
  in
  let arith_op, rel_op_lower, rel_op_upper =
    match direction with
    | Stmt.ForDirection.Forward ->
        (BinaryOperator.Plus, BinaryOperator.Ge, BinaryOperator.Lt)
    | Stmt.ForDirection.Backward ->
        (BinaryOperator.Minus, BinaryOperator.Le, BinaryOperator.Gt)
  in
  let in_range_expr =
    let counter_expr =
      Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
    in
    Expr.mk_binary BinaryOperator.And
      (Expr.mk_binary rel_op_lower counter_expr lower)
      (Expr.mk_binary rel_op_upper counter_expr upper)
  in
  let body = Stmt.mk_assume parent in_range_expr :: body in
  let counter0_expr = get_fresh_counter_expr "tmp_loop_cnt0" in
  let body0 = subst_loop_counter body counter0_expr in
  let counter1_expr = get_fresh_counter_expr "tmp_loop_cnt1" in
  let body1 = subst_loop_counter body counter1_expr in
  let lt_assume =
    Stmt.mk_assume parent
      (Expr.mk_binary BinaryOperator.Lt counter0_expr counter1_expr)
  in
  let case0 = List.append (lt_assume :: body0) body1 in
  let case1 = List.append (lt_assume :: body1) body0 in
  RuleHelper.create_equivalent_stmts_goal ast case0 case1

let apply_left ast env depth (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _} as
          s)
    :: rest
    when Expr.equal step expr_one ->
      let open Option in
      let parent = Stmt.parent_of s in
      create_legitimacy_goal ast parent counter lower upper direction body
      >>= fun (new_goal, ast) ->
      (* This rule makes the legitimacy assumption *)
      let new_goal = Goal.assign_blame (Some depth) new_goal in
      let lower', upper', direction' =
        match direction with
        | Stmt.ForDirection.Forward ->
            ( Expr.mk_binary BinaryOperator.Minus upper expr_one
            , Expr.mk_binary BinaryOperator.Minus lower expr_one
            , Stmt.ForDirection.Backward )
        | Stmt.ForDirection.Backward ->
            ( Expr.mk_binary BinaryOperator.Plus upper expr_one
            , Expr.mk_binary BinaryOperator.Plus lower expr_one
            , Stmt.ForDirection.Forward )
      in
      let new_for =
        Stmt.mk_for parent counter
          (* Simplify twice to pick up more const-folding opportunities *)
          (Expr.simplify (Expr.simplify lower'))
          (Expr.simplify (Expr.simplify upper'))
          expr_one direction' body
      in
      (* Don't forget to set the counter variable back to its correct value *)
      let cleanup_assign =
        Stmt.mk_assign parent
          (Lvalue.mk_var VarBinding.{name= counter; ty= Type.IntType})
          lower'
      in
      let left_stmts = new_for :: cleanup_assign :: rest in
      let curr_goal = Goal.replace goal ~left_stmts in
      Some ([new_goal; curr_goal], env, ast)
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_quasilocal_rule ~apply_left side in
  QuasiLocalRule.mk_rule ~is_applicable:(is_applicable side)
    ~is_aggressive:true ~name apply

let reverse_l = create "reverse_l" Side.Left

let reverse_r = create "reverse_r" Side.Right
