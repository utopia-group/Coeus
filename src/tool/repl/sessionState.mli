module HistoryState : sig
  type t = {prover_state: Prover.ProverState.t; prev_rule: Prover.Rule.t option}
end

type t = {file_path: string; ast: Ast.Ecoeus.t; history: HistoryState.t list}

val load_from_file : ?precise_arith:bool -> string -> (t, string) Core.Result.t

val add_prover_state : Prover.ProverState.t -> Prover.Rule.t -> t -> t

val current_state_of : t -> Prover.ProverState.t option

val current_goal_of : t -> Prover.Goal.t option

val rule_history_of : t -> Prover.Rule.t list
