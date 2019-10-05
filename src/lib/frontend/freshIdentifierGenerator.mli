open Ast.Coeus

module VarCollection : sig
  type t

  val create : ?size:int -> unit -> t

  val of_list : Identifier.t list -> t

  val add : t -> Identifier.t -> unit

  val mem : t -> Identifier.t -> bool
end

val get_fresh_local : string -> VarCollection.t -> Identifier.t

val rename_fresh_local : Identifier.t -> VarCollection.t -> Identifier.t
