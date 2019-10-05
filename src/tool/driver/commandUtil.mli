module Command : sig
  type t = int Cmdliner.Term.t * Cmdliner.Term.info
end

module SearchStrategy : sig
  type t =
    | Script
    | Random
    | SingleRolloutGuided
    | RepeatSingleRolloutGuided
    | SelfCompose
    | CompactSelfCompose
    | AlwaysSync
    | Descartes
    | SingleRolloutGuidedExhaustive
    | RandomGuidedExhaustive
    | DfsExhaustive
    | BfsExhaustive
end

val get_inet_addr : string option -> Core.Unix.Inet_addr.t Cmdliner.Term.ret

val random_state_term : Core.Random.State.t Cmdliner.Term.t

val setup_log_term : unit Cmdliner.Term.t

val setup_solver_term : Solver.SolverConfig.t Cmdliner.Term.t

val strategy_arg : SearchStrategy.t Cmdliner.Term.t

val frontend_config_term : Frontend.FrontendConfig.t Cmdliner.Term.t

val prover_config_term : Prover.ProverConfig.t Cmdliner.Term.t

val search_config_term : Searcher.SearchConfig.t Cmdliner.Term.t

val cache_file_arg : string option Cmdliner.Term.t

val cache_flush_freq_arg : int Cmdliner.Term.t
