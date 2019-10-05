type t =
     SearchConfig.t
  -> Prover.ProverState.t
  -> (Prover.Rule.t * Prover.ProverState.t) option

val rollout : t -> Strategy.t

val repeat_rollout : ?count:int -> t -> Strategy.t
