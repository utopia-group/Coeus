open Core
open Prover

type t = ProverConfig.t -> ProverState.t -> Rule.t option

let to_linear_strategy (rule_picker: t) config state =
  let prover_config = config.SearchConfig.prover_config in
  let open Option in
  rule_picker prover_config state
  >>= fun rule ->
  match ProverAction.apply_rule rule prover_config state with
  | Result.Ok state' -> Some (rule, state')
  | _ -> None
