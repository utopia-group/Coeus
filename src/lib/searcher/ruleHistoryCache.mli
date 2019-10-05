module RuleIndexList : sig
  type t = int list [@@deriving sexp, compare, hash]

  val to_rules : t -> Prover.Rule.t list

  val of_rules : Prover.Rule.t list -> (t, string) Core.Result.t

  val of_rules_exn : Prover.Rule.t list -> t
end

module Key : sig
  type t = int * RuleIndexList.t [@@deriving sexp, compare, hash]
end

module type S = sig
  type 'a t

  val get : 'a t -> key:Key.t -> 'a option

  val set : 'a t -> key:Key.t -> data:'a -> unit

  val to_alist : 'a t -> (Key.t * 'a) list

  val length : 'a t -> int
end

module KeepAll : sig
  include S

  val create : unit -> 'a t
end

module Lru : sig
  include S

  val create : size:int -> 'a t
end

module MakeSerializer (C : S) (V : Core.Sexpable.S) : sig
  type data_t = V.t [@@deriving sexp]

  type cache_t = data_t C.t

  val load : string -> cache_t -> unit

  val store : cache_t -> string -> unit
end

val memoize : ?lru_size:int -> f:(Key.t -> 'a) -> Key.t -> 'a
