open Core
open Prover

let list_shuffle rand_state l =
  let rec shuffle = function
    | [] -> []
    | [single] -> [single]
    | list ->
        let before, after =
          List.partition_tf ~f:(fun _ -> Random.State.bool rand_state) list
        in
        List.rev_append (shuffle before) (shuffle after)
  in
  shuffle l

let create rand_state config prover_state =
  let rule_states =
    ProverAction.applicable_candidate_rules_with_states
      config.SearchConfig.prover_config prover_state
  in
  if List.is_empty rule_states then [] else list_shuffle rand_state rule_states
