open Core
open Prover
open Searcher

type t = {search_state: ExhaustiveSearchState.t}

let load_from_file config file_path =
  let open Result in
  Ast.CoeusParselib.parse_file file_path
  >>= fun prog ->
  Frontend.Pipeline.run_default config prog
  >>= fun ast ->
  let open Ast.Coeus in
  let prover_state = ProverState.init ast in
  let search_state = ExhaustiveSearchState.init prover_state in
  Ok {search_state}

let history_of {search_state} = ExhaustiveSearchState.to_rules search_state

let prover_state_of {search_state} = search_state.prover_state

let search_state_of {search_state} = search_state

let update config rule {search_state} =
  let open Result in
  let prover_state = search_state.prover_state in
  match ProverState.current_goal_of prover_state with
  | None ->
      let msg = "Cannot update fully resolved ProverState in RolloutSession" in
      Error msg
  | Some goal ->
      let prev_depth = goal.Goal.prev_step in
      ProverAction.apply_rule rule config search_state.prover_state
      >>= fun prover_state ->
      let subgoal_index = ProverState.current_subgoal_index_of prover_state in
      let step =
        ExhaustiveSearchState.ProofStep.{rule; prev_depth; subgoal_index}
      in
      let rev_history = step :: search_state.rev_history in
      let search_state = ExhaustiveSearchState.{prover_state; rev_history} in
      Ok {search_state}

let try_discharge search_config {search_state} =
  ExhaustiveSearchState.try_discharge search_config search_state
