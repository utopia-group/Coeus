type t =
     SearchConfig.t
  -> Prover.ProverState.t
  -> (Prover.Rule.t * Prover.ProverState.t) list

module ExpandStatus : sig
  type ('a, 'b) t = Terminal of 'a | NonTerminal of 'b
end

module type S = sig
  (** Local search state *)
  type t

  (** Global search state *)
  type u

  val init : SearchConfig.t -> Prover.ProverState.t -> u * t

  val compare : t -> t -> int

  val expand : u -> t -> (SearchResult.t, t list) ExpandStatus.t
end

module type L = sig
  type t

  val init : SearchConfig.t -> Prover.ProverState.t -> t

  val compare : t -> t -> int

  val search_state_of : t -> ExhaustiveSearchState.t

  val expand :
    int option -> t -> SearchConfig.t -> Prover.ProverState.t -> t list
end

module MakeConflictState (LocalState : L) : S with type t = LocalState.t

module type W = functor (State : S) -> sig
  (** Type of the working set *)
  type t

  val init : State.t -> t

  val add : t -> State.t -> unit

  val take : t -> State.t option
end

module Make (WorkSetMaker : W) (State : S) : sig
  val search :
       SearchConfig.t
    -> Prover.ProverState.t
    -> (SearchResult.t, string) Core.Result.t
end

val dfs : t -> Strategy.t

val bfs : t -> Strategy.t
