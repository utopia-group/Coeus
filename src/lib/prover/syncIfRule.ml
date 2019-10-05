open Core
open Ast.Ecoeus
open Verifier

let is_applicable _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | Stmt.({bare_stmt= If _; _}) :: _, Stmt.({bare_stmt= If _; _}) :: _ -> true
  | _ -> false

let do_apply left_cond left_then left_else left_rest right_cond right_then
    right_else right_rest env depth (goal: Goal.t) =
  let eq_cond = Expr.mk_binary BinaryOperator.Eq left_cond right_cond in
  let eq_preconds =
    Precondition.mk_assume [Expr.logical_negate_of eq_cond] :: goal.pre_conds
  in
  let eq_assert =
    Postcondition.mk_assert ~is_final:false
      [Expr.mk_literal (Literal.BoolLit false)]
  in
  let eq_goal =
    Goal.replace goal ~left_stmts:[] ~right_stmts:[] ~pre_conds:eq_preconds
      ~post_cond:eq_assert
  in
  (* Eq goal, if unsat, can be blamed on this rule *)
  let eq_goal = Goal.assign_blame (Some depth) eq_goal in
  let then_assume = Precondition.mk_assume [left_cond; right_cond] in
  let then_goal =
    Goal.replace goal
      ~left_stmts:(List.append left_then left_rest)
      ~right_stmts:(List.append right_then right_rest)
      ~pre_conds:(then_assume :: goal.pre_conds)
  in
  let else_assume =
    Precondition.mk_assume
      [Expr.logical_negate_of left_cond; Expr.logical_negate_of right_cond]
  in
  let else_goal =
    Goal.replace goal
      ~left_stmts:(List.append left_else left_rest)
      ~right_stmts:(List.append right_else right_rest)
      ~pre_conds:(else_assume :: goal.pre_conds)
  in
  Some ([eq_goal; then_goal; else_goal], env)

let apply reversed _ env depth (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | ( Stmt.({ bare_stmt=
                If
                  { cond= left_cond
                  ; then_branch= left_then_branch
                  ; else_branch= left_else_branch }; _ })
      :: left_rest
    , Stmt.({ bare_stmt=
                If
                  { cond= right_cond
                  ; then_branch= right_then_branch
                  ; else_branch= right_else_branch }; _ })
      :: right_rest ) ->
      if reversed then
        let right_cond = Expr.logical_negate_of right_cond in
        do_apply left_cond left_then_branch left_else_branch left_rest
          right_cond right_else_branch right_then_branch right_rest env depth
          goal
      else
        do_apply left_cond left_then_branch left_else_branch left_rest
          right_cond right_then_branch right_else_branch right_rest env depth
          goal
  | _ -> None

let syncif =
  SemiLocalRule.mk_rule ~is_applicable ~is_aggressive:true ~name:"syncif"
    (apply false)

let rsyncif =
  SemiLocalRule.mk_rule ~is_applicable ~is_aggressive:true ~name:"rsyncif"
    (apply true)
