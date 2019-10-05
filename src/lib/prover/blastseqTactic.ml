open Core
open Ast.Ecoeus

let is_applicable _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | s0 :: _, s1 :: _
    when (Stmt.is_loop s0 || Stmt.is_call s0)
         && (Stmt.is_loop s1 || Stmt.is_call s1) ->
      (* This is the only case where BlastSeq will not make any progress *)
      false
  | _ -> true

let apply _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | _ :: _, [] -> Some SeqRule.seq_l
  | s :: _, _ when not (Stmt.is_loop s || Stmt.is_call s) -> Some SeqRule.seq_l
  | [], _ :: _ -> Some SeqRule.seq_r
  | _, s :: _ when not (Stmt.is_loop s || Stmt.is_call s) -> Some SeqRule.seq_r
  | _, _ -> None

let blastseq =
  LocalTactic.mk_shallow_rule ~is_applicable ~name:"blastseq" apply
