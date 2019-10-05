module FlushConfig : sig
  type t = {file: string option; flush_freq: int}
end

module CallWithCache : sig
  (** Return the corresponding entry if [key] hits the cache, or it will invoke [f key] for cache misses. The first return value of [f] determines whether the second value will be inserted into the cache or not *)
  type ('k, 'a) t = f:('k -> bool * 'a) -> 'k -> 'a
end

module MakeLocal (C : RuleHistoryCache.S) (V : Core.Sexpable.S) : sig
  type t

  val create : FlushConfig.t -> V.t C.t -> t

  val call_with_cache : t -> (RuleHistoryCache.Key.t, V.t) CallWithCache.t
end

module MakeRemote (C : RuleHistoryCache.S) (V : Core.Sexpable.S) : sig
  type t

  val create : Core.In_channel.t -> Core.Out_channel.t -> t

  val call_with_cache : t -> (RuleHistoryCache.Key.t, V.t) CallWithCache.t
end
