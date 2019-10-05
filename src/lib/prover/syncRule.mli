module SyncSide : sig
  type t = Neither | Both | Left | Right
end

val create : string -> SyncSide.t -> Rule.t

val sync : Rule.t

val sync_l : Rule.t

val sync_r : Rule.t

val sync_n : Rule.t
