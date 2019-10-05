type t =
     SearchConfig.t
  -> Prover.ProverState.t
  -> (SearchResult.t, string) Core.Result.t
