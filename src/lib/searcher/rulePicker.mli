type t = Prover.ProverConfig.t -> Prover.ProverState.t -> Prover.Rule.t option

val to_linear_strategy : t -> LinearStrategy.t
