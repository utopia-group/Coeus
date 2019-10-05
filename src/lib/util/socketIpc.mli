type handler = Core.In_channel.t -> Core.Out_channel.t -> bool

val establish_server : handler -> Core.Unix.sockaddr -> unit

val connect_server : handler -> Core.Unix.sockaddr -> unit

val establish_remote_server : handler -> Core.Unix.Inet_addr.t -> int -> unit

val connect_remote_server : handler -> Core.Unix.Inet_addr.t -> int -> unit

val establish_local_server : handler -> string -> unit

val read_remote_sexp :
     Core.In_channel.t
  -> of_sexp:(Core.Sexp.t -> 'a)
  -> ('a, string) Core.Result.t

val write_remote_sexp :
  Core.Out_channel.t -> sexp_of:('a -> Core.Sexp.t) -> 'a -> unit
