type t

val init : Config.t -> t

val start_new_session : t -> int -> (RolloutSession.t, string) Core.Result.t

val restart_session : t -> (RolloutSession.t, string) Core.Result.t

val close_session : t -> unit

val get_current_benchmark_id : t -> int option

val get_current_session : t -> (RolloutSession.t, string) Core.Result.t

val update_current_session :
  t -> RolloutSession.t -> (unit, string) Core.Result.t

val add_conflict : t -> int list -> unit

val check_conflict : t -> Prover.Rule.t -> Prover.ProverState.t -> bool

val get_config : t -> Config.t
