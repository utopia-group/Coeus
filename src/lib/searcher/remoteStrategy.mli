type t =
     (   SearchConfig.t
      -> Prover.ProverState.t
      -> ( (Prover.Rule.t * Prover.ProverState.t * float) list
         , string )
         Core.Result.t)
  -> Strategy.t

val to_strategy : t -> Core.Unix.Inet_addr.t * int -> Strategy.t
