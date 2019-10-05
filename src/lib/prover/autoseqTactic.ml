open Core
open Verifier
open Ast.Ecoeus

let can_advance = function
  | [] -> false
  | s :: _ when Stmt.is_simple s -> true
  | _ -> false

let is_applicable _ (goal: Goal.t) =
  can_advance goal.left_stmts || can_advance goal.right_stmts

let local_tactic _ (goal: Goal.t) =
  if can_advance goal.left_stmts then Some SeqRule.seq_l
  else if can_advance goal.right_stmts then Some SeqRule.seq_r
  else None

let autoseq =
  LocalTactic.mk_shallow_rule ~is_applicable ~name:"autoseq" local_tactic
