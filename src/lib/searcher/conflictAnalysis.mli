module Trace : sig
  type t = {rule: Prover.Rule.t; subgoal_index: int option}
end

module ConflictSpec : sig
  type t

  val spec_match : Trace.t list -> t -> bool

  val pp : t Fmt.t
end

module ConflictSpecs : sig
  type t

  val specs_match : Trace.t list list -> t -> bool

  val pp : t Fmt.t
end

module ConflictCache : sig
  type t

  val create : size:int -> t

  val add_specs : t -> ConflictSpecs.t -> unit

  val lookup : t -> Trace.t list list -> bool

  val length : t -> int

  val max_size : t -> int

  val clear : t -> unit
end

val extract_conflict_specs :
  ExhaustiveSearchState.ProofStep.t list -> int list -> ConflictSpecs.t

val add_conflict :
  ConflictCache.t -> ExhaustiveSearchState.t -> int list -> unit

val check_conflict : ConflictCache.t -> ExhaustiveSearchState.t -> bool
