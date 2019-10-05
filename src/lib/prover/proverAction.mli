val apply_rule :
     Rule.t
  -> ProverConfig.t
  -> ProverState.t
  -> (ProverState.t, string) Core.Result.t

(* val applicable_candidate_rules : ProverConfig.t -> ProverState.t -> Rule.t list *)
(* val applicable_candidate_rule_indices :
 *   ProverConfig.t -> ProverState.t -> int list *)

val applicable_candidate_rules_with_states :
  ProverConfig.t -> ProverState.t -> (Rule.t * ProverState.t) list

val applicable_candidate_rule_indices_with_states :
  ProverConfig.t -> ProverState.t -> (int * ProverState.t) list

val prune_goal :
     Solver.SolverConfig.t
  -> ProverState.t
  -> (ProverState.t, string) Core.Result.t
(** Return a new state with the active goal pruned iff the goal can be trivially proven (i.e. contradictions in the precondition) *)

val discharge :
     Solver.SolverConfig.t
  -> ProverState.t
  -> (Solver.Status.t, string) Core.Result.t

val is_state_provable : Solver.SolverConfig.t -> ProverState.t -> bool
