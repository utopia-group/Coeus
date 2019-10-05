open Core
open Ast.Ecoeus

let exprs_depend es name =
  let varset = Expr.free_var_set_of_exprs es in
  Hash_set.mem varset VarBinding.{name; ty= Type.IntType}

let process_counter counter upper lower step direction =
  let counter_expr =
    Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
  in
  let arith_op, rel_op_lower, rel_op_upper =
    match direction with
    | Stmt.ForDirection.Forward ->
        (BinaryOperator.Plus, BinaryOperator.Ge, BinaryOperator.Lt)
    | Stmt.ForDirection.Backward ->
        (BinaryOperator.Minus, BinaryOperator.Le, BinaryOperator.Gt)
  in
  let in_range_expr0 =
    Expr.mk_binary BinaryOperator.And
      (Expr.mk_binary rel_op_lower counter_expr lower)
      (Expr.mk_binary rel_op_upper counter_expr upper)
  in
  let next_counter_expr = Expr.mk_binary arith_op counter_expr step in
  (next_counter_expr, in_range_expr0)

let singleton_map v e v' = if Identifier.equal v v' then Some e else None

let create_legitimacy_goal ast parent counter0 lower0 upper0 step0 direction0
    counter1 lower1 upper1 step1 direction1 body =
  let counter'_e0, in_range_e0 =
    process_counter counter0 lower0 upper0 step0 direction0
  in
  let counter'_e1, in_range_e1 =
    process_counter counter1 lower1 upper1 step1 direction1
  in
  let body =
    Stmt.mk_assume parent in_range_e0
    :: Stmt.mk_assume parent in_range_e1
    :: body
  in
  let body_upper =
    Stmts.map_expr body ~f:(Expr.subst ~f:(singleton_map counter0 counter'_e0))
  in
  let body_lower =
    Stmts.map_expr body ~f:(Expr.subst ~f:(singleton_map counter1 counter'_e1))
  in
  let stmts0 = List.append body_upper body_lower in
  let stmts1 = List.append body_lower body_upper in
  RuleHelper.create_equivalent_stmts_goal ast stmts0 stmts1

let do_interchange counter0 lower0 upper0 step0 direction0 counter1 lower1
    upper1 step1 direction1 body left_stmts ast parent env depth goal =
  let open Option in
  create_legitimacy_goal ast parent counter0 lower0 upper0 step0 direction0
    counter1 lower1 upper1 step1 direction1 body
  >>= fun (new_goal, ast) ->
  (* This rule makes the legitimacy assumption *)
  let new_goal = Goal.assign_blame (Some depth) new_goal in
  let new_for =
    Stmt.mk_for parent counter1 lower1 upper1 step1 direction1
      [Stmt.mk_for parent counter0 lower0 upper0 step0 direction0 body]
  in
  let left_stmts = new_for :: left_stmts in
  let goal' = Goal.replace goal ~left_stmts in
  Some ([new_goal; goal'], env, ast)

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with
  | Stmt.({ bare_stmt=
              For
                { counter= counter0
                ; body=
                    [ Stmt.({ bare_stmt=
                                For
                                  {lower= lower1; upper= upper1; step= step1; _}; _
                            }) ]; _ }; _ })
    :: _
    when not (exprs_depend [lower1; upper1; step1] counter0) ->
      true
  | _ -> false

let apply_left ast env depth (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({ bare_stmt=
              For
                { counter= counter0
                ; lower= lower0
                ; upper= upper0
                ; step= step0
                ; direction= direction0
                ; body=
                    [ Stmt.({ bare_stmt=
                                For
                                  { counter= counter1
                                  ; lower= lower1
                                  ; upper= upper1
                                  ; step= step1
                                  ; direction= direction1
                                  ; body }; _ }) ] }; _ })
    :: left_stmts
    when not (exprs_depend [lower1; upper1; step1] counter0) ->
      do_interchange counter0 lower0 upper0 step0 direction0 counter1 lower1
        upper1 step1 direction1 body left_stmts ast goal.left_proc env depth
        goal
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_quasilocal_rule ~apply_left side in
  QuasiLocalRule.mk_rule ~name ~is_applicable:(is_applicable side)
    ~is_aggressive:true apply

let interchange_l = create "interchange_l" Side.Left

let interchange_r = create "interchange_r" Side.Right
