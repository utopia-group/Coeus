type t

val load_from_file :
  Frontend.FrontendConfig.t -> string -> (t, string) Core.Result.t

val history_of : t -> Prover.Rule.t list

val prover_state_of : t -> Prover.ProverState.t

val search_state_of : t -> Searcher.ExhaustiveSearchState.t

val update :
  Prover.ProverConfig.t -> Prover.Rule.t -> t -> (t, string) Core.Result.t

val try_discharge :
     Searcher.SearchConfig.t
  -> t
  -> Searcher.ExhaustiveSearchState.DischargeResult.t
