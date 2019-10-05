open Core
open Ast.Ecoeus
open Verifier

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with stmt :: _ when Stmt.is_loop stmt -> true | _ -> false

let update_for_once lower step = function
  | Stmt.ForDirection.Forward ->
      Expr.simplify (Expr.mk_binary BinaryOperator.Plus lower step)
  | Stmt.ForDirection.Backward ->
      Expr.simplify (Expr.mk_binary BinaryOperator.Minus lower step)

let peel_for_cond lower upper = function
  | Stmt.ForDirection.Forward ->
      Expr.simplify (Expr.mk_binary BinaryOperator.Lt lower upper)
  | Stmt.ForDirection.Backward ->
      Expr.simplify (Expr.mk_binary BinaryOperator.Gt lower upper)

let do_peel peel_cond peel_body loop_stmt rest env (goal: Goal.t) =
  let neg_cond = Expr.logical_negate_of peel_cond in
  let pos_pre = Precondition.mk_assume [peel_cond] in
  let neg_pre = Precondition.mk_assume [neg_cond] in
  let pos_pres = pos_pre :: goal.pre_conds in
  let neg_pres = neg_pre :: goal.pre_conds in
  let neg_stmts = loop_stmt :: rest in
  let pos_stmts = List.append peel_body neg_stmts in
  let pos_goal = Goal.replace goal ~pre_conds:pos_pres ~left_stmts:pos_stmts in
  let neg_goal = Goal.replace goal ~pre_conds:neg_pres ~left_stmts:neg_stmts in
  Some ([pos_goal; neg_goal], env)

let apply_left _ env _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({bare_stmt= While {cond; body}; _} as s) :: rest ->
      do_peel cond body s rest env goal
  | Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _} as
          s)
    :: rest ->
      let body' =
        let subst_f v =
          if Identifier.equal v counter then Some lower else None
        in
        List.map body ~f:(Stmt.map_expr ~f:(Expr.subst ~f:subst_f))
      in
      let cond = peel_for_cond lower upper direction in
      let lower = update_for_once lower step direction in
      let for_stmt' =
        let parent = Stmt.parent_of s in
        Stmt.mk_for parent counter lower upper step direction body
      in
      do_peel cond body' for_stmt' rest env goal
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_semilocal_rule ~apply_left side in
  SemiLocalRule.mk_rule ~is_applicable:(is_applicable side) ~name apply

let peel_l = create "peel_l" Side.Left

let peel_r = create "peel_r" Side.Right
