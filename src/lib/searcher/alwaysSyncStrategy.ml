open Core
open Ast.Ecoeus

let pick_rule _ Prover.ProverState.({goals; _}) =
  let open Prover in
  match goals with
  | [] -> None
  | Goal.({left_stmts; right_stmts; _}) :: _ ->
    match (left_stmts, right_stmts) with
    | [], [] ->
        (* This point should not be reached normally: the client should have no way of accessing partially-resolved proof states *)
        Logs.warn (fun m ->
            m "AlwaysSyncStrategy.next_rule reached a partially-resolved goal"
        ) ;
        None
    | left_stmt :: _, right_stmt :: _
      when Stmt.is_loop left_stmt && Stmt.is_loop right_stmt ->
        Some SyncRule.sync
    | _, _ -> Some BlastseqTactic.blastseq

let create () = RulePicker.to_linear_strategy pick_rule
