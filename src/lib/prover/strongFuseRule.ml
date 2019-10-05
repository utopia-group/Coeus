open Core
open Ast.Ecoeus

let expr_one = Expr.mk_literal (Literal.IntLit Bigint.one)

let create_legitimacy_goal ast parent counter0 lower0 upper0 counter1 lower1
    upper1 direction body0 body1 =
  let subst_loop_counter body counter expr =
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
  let in_range_expr counter lower upper =
    let counter_expr =
      Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
    in
    Expr.mk_binary BinaryOperator.And
      (Expr.mk_binary rel_op_lower counter_expr lower)
      (Expr.mk_binary rel_op_upper counter_expr upper)
  in
  let mk_body counter lower upper body =
    Stmt.mk_assume parent (in_range_expr counter lower upper) :: body
  in
  let counter0_expr = get_fresh_counter_expr "tmp_loop_cnt0" in
  let body0 =
    subst_loop_counter
      (mk_body counter0 lower0 upper0 body0)
      counter0 counter0_expr
  in
  let counter1_expr = get_fresh_counter_expr "tmp_loop_cnt1" in
  let body1 =
    subst_loop_counter
      (mk_body counter1 lower1 upper1 body1)
      counter1 counter1_expr
  in
  let gt_assume =
    Stmt.mk_assume parent
      (Expr.mk_binary BinaryOperator.Gt counter0_expr counter1_expr)
  in
  let case0 = List.append (gt_assume :: body0) body1 in
  let case1 = List.append (gt_assume :: body1) body0 in
  let open Option in
  RuleHelper.create_equivalent_stmts_goal ast case0 case1
  >>| fun (goal, ast) ->
  let pre_cond =
    Verifier.Precondition.mk_assume
      [Expr.mk_binary BinaryOperator.Gt counter0_expr counter1_expr]
  in
  let pre_conds = pre_cond :: goal.pre_conds in
  let goal = Goal.replace goal ~pre_conds in
  (goal, ast)

let apply_left ast env depth (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({ bare_stmt=
              For
                { counter= counter0
                ; lower= lower0
                ; upper= upper0
                ; step= step0
                ; direction= direction0
                ; body= body0 }; _ })
    :: Stmt.({ bare_stmt=
                 For
                   { counter= counter1
                   ; lower= lower1
                   ; upper= upper1
                   ; step= step1
                   ; direction= direction1
                   ; body= body1 }; _ })
       :: left_stmts
    when Expr.equal (Expr.simplify lower0) (Expr.simplify lower1)
         && Expr.equal (Expr.simplify upper0) (Expr.simplify upper1)
         && Expr.equal (Expr.simplify step0) (Expr.simplify step1)
         && Expr.equal (Expr.simplify step0) expr_one
         && Stmt.ForDirection.equal direction0 direction1 ->
      let open Option in
      create_legitimacy_goal ast goal.left_proc counter0 lower0 upper0 counter1
        lower1 upper1 direction0 body0 body1
      >>= fun (new_goal, ast) ->
      let new_goal = Goal.assign_blame (Some depth) new_goal in
      let left_stmts =
        FuseRule.fuse_for_loops goal.left_proc counter0 counter1 lower0 upper0
          step0 direction0 body0 body1 left_stmts
      in
      let curr_goal = Goal.replace goal ~left_stmts in
      Some ([new_goal; curr_goal], env, ast)
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_quasilocal_rule ~apply_left side in
  QuasiLocalRule.mk_rule ~is_aggressive:true ~name apply

let sfuse_l = create "sfuse_l" Side.Left

let sfuse_r = create "sfuse_r" Side.Right
