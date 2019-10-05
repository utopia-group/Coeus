open Core
open Prover
module SearchState = ExhaustiveSearchState

module PriorityWorkSet (State : TreeStrategy.S) = struct
  type t = State.t Heap.t

  let init state =
    let prio_queue = Heap.create State.compare () in
    Heap.add prio_queue state ; prio_queue

  let add = Heap.add

  let take = Heap.pop
end

let gen_local_state remote_guide =
  ( module struct
    type t = {search_state: ExhaustiveSearchState.t; prio: float}

    let compare lhs rhs =
      (* Larger priority is better *)
      Float.compare rhs.prio lhs.prio

    let init _ prover_state =
      let search_state = ExhaustiveSearchState.init prover_state in
      let prio = Float.log 1.0 in
      {search_state; prio}

    let search_state_of {search_state; _} = search_state

    let expand prev_depth {search_state; prio} search_config prover_state =
      let transitions =
        match remote_guide search_config prover_state with
        | Result.Error msg ->
            Logs.info (fun m -> m "Remote rule picking failed: %s" msg) ;
            []
        | Result.Ok transitions -> transitions
      in
      let open ExhaustiveSearchState in
      List.map transitions ~f:(fun (rule, prover_state, prob) ->
          let subgoal_index =
            ProverState.current_subgoal_index_of prover_state
          in
          let step = SearchState.ProofStep.{rule; prev_depth; subgoal_index} in
          let rev_history = step :: search_state.rev_history in
          let search_state = SearchState.{prover_state; rev_history} in
          let prio = prio +. Float.log prob in
          {search_state; prio} )
  end
  : TreeStrategy.L )

let remote_strategy remote_guide =
  let module LocalState = (val gen_local_state remote_guide) in
  let module SearchState = TreeStrategy.MakeConflictState (LocalState) in
  let module Search = TreeStrategy.Make (PriorityWorkSet) (SearchState) in
  Search.search

let create = RemoteStrategy.to_strategy remote_strategy

let random_remote_guide rand_state search_config prover_state =
  let prover_config = search_config.SearchConfig.prover_config in
  let next_rule_states =
    ProverAction.applicable_candidate_rules_with_states prover_config
      prover_state
  in
  let next_rule_length = List.length next_rule_states in
  if next_rule_length = 0 then
    let msg = Fmt.strf "Cannot find any available rules" in
    Result.Error msg
  else
    let next_rule_weights =
      List.init next_rule_length ~f:(fun _ ->
          1 + Random.State.int rand_state 999 )
    in
    let next_rule_probs =
      let wsum =
        List.fold ~init:0 next_rule_weights ~f:(fun acc w -> acc + w)
      in
      let wsum_float = Float.of_int wsum in
      List.map next_rule_weights ~f:(fun w -> Float.of_int w /. wsum_float)
    in
    let next =
      List.map (List.zip_exn next_rule_states next_rule_probs)
        ~f:(fun ((r, s), i) -> (r, s, i) )
    in
    Result.Ok next

let create_random rand_state = remote_strategy (random_remote_guide rand_state)
