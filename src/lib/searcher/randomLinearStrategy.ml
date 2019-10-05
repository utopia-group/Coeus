open Core
open Ast.Coeus
open Prover

let create rand_state config state =
  let prover_config = config.SearchConfig.prover_config in
  let applicable_rule_states =
    Array.fold Rules.candidate_rules ~init:[] ~f:(fun acc r ->
        match ProverAction.apply_rule r prover_config state with
        | Result.Ok state' -> (r, state') :: acc
        | _ -> acc )
  in
  if List.is_empty applicable_rule_states then None
  else
    (* Logs.debug (fun m ->
     *     m "# Available Rules = %d" (List.length applicable_rule_states) ) ; *)
    let n = Random.State.int rand_state (List.length applicable_rule_states) in
    let res = List.nth_exn applicable_rule_states n in
    Some res
