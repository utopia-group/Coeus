(** Generic caching library. Directly copied from Core_extended and add support for to_alist and length. *)

open Core

val memoize :
     ?destruct:('b -> unit)
  -> ?expire:[`Lru of int | `Keep_all | `Keep_one]
  -> ('a -> 'b)
  -> 'a
  -> 'b
(** [memoize ~destruct ~expire f]
    memoizes the results of [f].

    @param expire Strategy used to prune out values from the cache
    - [`Keep_one]: only keeps the last result around
    - [`Keep_all]: (the default value) never delete any values from the cache
    - [`Lru n]: keep [n] values in the cache and them removes the least recently
    used

    @param destruct function called on every value we remove from the cache
*)

val unit : (unit -> 'a) -> unit -> 'a
(** Returns memoized version of any function with argument unit. In effect this
    builds a lazy value.*)

(** {1 Exposed cache }

    These modules implement memoization and give you access to the cache. This,
    for instance, enables you to flush it.
*)

(** Least recently used caching *)
module Lru : sig
  type ('k, 'v) t

  type ('a, 'b) memo = ('a, ('b, exn) Result.t) t

  val find : ('k, 'v) t -> 'k -> 'v option

  val add : ('k, 'v) t -> key:'k -> data:'v -> unit

  val remove : ('k, _) t -> 'k -> unit

  val clear : (_, _) t -> unit

  val create : destruct:('v -> unit) option -> int -> ('k, 'v) t

  val call_with_cache : cache:('a, 'b) memo -> ('a -> 'b) -> 'a -> 'b

  val to_alist : ('k, 'v) t -> ('k * 'v) list

  val length : (_, _) t -> int

  val memoize :
    ?destruct:('b -> unit) -> ('a -> 'b) -> int -> ('a, 'b) memo * ('a -> 'b)
end

(** Full caching (never flushes out values automatically ) *)
module Keep_all : sig
  type ('k, 'v) t

  type ('a, 'b) memo = ('a, ('b, exn) Result.t) t

  val find : ('k, 'v) t -> 'k -> 'v option

  val add : ('k, 'v) t -> key:'k -> data:'v -> unit

  val remove : ('k, _) t -> 'k -> unit

  val clear : (_, _) t -> unit

  val create : destruct:('v -> unit) option -> ('k, 'v) t

  val call_with_cache : cache:('a, 'b) memo -> ('a -> 'b) -> 'a -> 'b

  val to_alist : ('k, 'v) t -> ('k * 'v) list

  val length : (_, _) t -> int

  val memoize :
    ?destruct:('b -> unit) -> ('a -> 'b) -> ('a, 'b) memo * ('a -> 'b)
end

(** {1  Generic caching}

    This enables you to implement your own caching strategy and store.

    Generic caching is based on separating the replacement policie and the
    store and tying them together with [Make].
*)

(** Replacement policy

    This dictates when elements will droped from the cache.
*)
module type Strategy = sig
  type 'a t

  (** This type is used to specify the signature of [cps_create]. For instance
      if [cps_create] takes two arguments of types [x] and [y]:
{[
  type 'a with_init_args : x -> y -> 'a
]}
  *)
  type 'a with_init_args

  val cps_create : f:(_ t -> 'b) -> 'b with_init_args
  (** [cps_create ~f ] is given in CPS form to enable chaining. (i.e. instead of
      directly returning a value it applies f to this value). *)

  val touch : 'a t -> 'a -> 'a list
  (** Marks an element as "fresh". Returns a list of elements to be dropped from
      the store. *)

  val remove : 'a t -> 'a -> unit
  (** Informs the strategy that an element was removed from the store. *)

  val clear : 'a t -> unit
  (** Inform the strategy that all the elements where dropped from the store. *)
end

(** Caching store

    A [Store] is the backend used to store the values in a cache. A store is
    a key/value associative table.
*)
module type Store = sig
  (** A key value store. *)
  type ('k, 'v) t

  type 'a with_init_args

  val cps_create : f:((_, _) t -> 'b) -> 'b with_init_args
  (** [cps_create] is given in CPS form to enable chaining.

      see {!Cache.Strategy.cps_create} for more information.
  *)

  val clear : ('k, 'v) t -> unit
  (** Remove all the values from the store. *)

  val set : ('k, 'v) t -> key:'k -> data:'v -> unit
  (** [set store ~key ~data] associated the [data] to [key]; remove any
      previously existing binding. *)

  val find : ('k, 'v) t -> 'k -> 'v option
  (** [find store key] returns the value associated to [key] in [store].  *)

  val data : (_, 'v) t -> 'v list
  (** [data store] returns all values in [store]. *)

  val to_alist : ('k, 'v) t -> ('k * 'v) list
  (** [to_alist store] returns all key-value pairs in [store] *)

  val length : (_, _) t -> int
  (** [length store] returns the size of [store] *)

  val remove : ('k, 'v) t -> 'k -> unit
  (** [remove store key] removes the binding for [key] in [store]. *)
end

(** The output signature of the functor {!Cache.Make} *)
module type S = sig
  (** A key value cache*)
  type ('k, 'v) t

  (** Used to specify the type of the {!create} and {!memoize} function. This
      describes the arguments required to initialise the caching strategy and
      the store. For instance if the store doesn't take any argument (eg.:
      {!Store.Table}) and the strategy takes an [int] (eg.: {!Strategy.Lru})
      this type will be:

{[
   type 'a with_init_args = int -> 'a
]}
  *)
  type 'a with_init_args

  type ('a, 'b) memo = ('a, ('b, exn) Result.t) t

  val find : ('k, 'v) t -> 'k -> 'v option

  val add : ('k, 'v) t -> key:'k -> data:'v -> unit

  val remove : ('k, _) t -> 'k -> unit

  val clear : (_, _) t -> unit

  val create : destruct:('v -> unit) option -> ('k, 'v) t with_init_args

  val call_with_cache : cache:('a, 'b) memo -> ('a -> 'b) -> 'a -> 'b

  val to_alist : ('k, 'v) t -> ('k * 'v) list

  val length : (_, _) t -> int

  val memoize :
       ?destruct:('b -> unit)
    -> ('a -> 'b)
    -> (('a, 'b) memo * ('a -> 'b)) with_init_args
end

(** Predefined strategies *)
module Strategy : sig
  (** Least recently used. *)
  module Lru : Strategy with type 'a with_init_args = int -> 'a

  (** Keep all the values*)
  module Keep_all : Strategy with type 'a with_init_args = 'a
end

(** Predefined stores *)
module Store : sig
  module Table : Store with type 'a with_init_args = 'a
end

module Make (Strat : Strategy) (Store : Store) :
  S with type 'a with_init_args = 'a Store.with_init_args Strat.with_init_args
