val create : Core.Random.State.t -> Core.Unix.Inet_addr.t * int -> Strategy.t

val create_repeat :
     ?count:int
  -> Core.Random.State.t
  -> Core.Unix.Inet_addr.t * int
  -> Strategy.t
