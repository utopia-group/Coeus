open Core
open Ast.Ecoeus
open Verifier

let is_applicable _ _ = true

let apply _ depth (goal: Goal.t) =
  let post_cond =
    Postcondition.mk_assert ~is_final:false
      [Expr.mk_literal (Literal.of_bool false)]
  in
  let goal' = Goal.replace goal ~left_stmts:[] ~right_stmts:[] ~post_cond in
  (* This rule makes the strong assumption that the current goal is trivially sat *)
  let goal'' = Goal.assign_blame (Some depth) goal' in
  Some goal''

let contra =
  LocalRule.mk_rule ~name:"contra" ~is_applicable ~is_aggressive:true apply
