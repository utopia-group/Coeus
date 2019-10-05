open Core
open Ast.Coeus

let pick_rule _ Prover.ProverState.({goals; _}) =
  let open Prover in
  match goals with
  | [] -> None
  | Goal.({left_stmts; right_stmts; _}) :: _ ->
    match (left_stmts, right_stmts) with
    | [], [] ->
        (* This point should not be reached normally: the client should have no way of accessing partially-resolved proof states *)
        Logs.warn (fun m ->
            m
              "NaiveSelfComposeStrategy.next_rule reached a \
               partially-resolved goal" ) ;
        None
    | [], _ -> Some SwapRule.swap
    | _, _ -> Some SeqRule.seq_l

let create () = RulePicker.to_linear_strategy pick_rule
