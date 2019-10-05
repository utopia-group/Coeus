type t

val set : key:string -> ?value:string -> t -> t

val set_batch : (string * string option) list -> t -> t

val unset : key:string -> t -> t

val unset_batch : keys:string list -> t -> t

val mem : key:string -> t -> bool

val lookup : key:string -> t -> string option

val lookup_int : key:string -> t -> int option

val lookup_exn : key:string -> t -> string

val empty : t

val of_alist : (string * string) list -> t

val to_alist : t -> (string * string) list
