val get_fresh_inv : ?rel:bool -> unit -> Ast.Coeus.Identifier.t

val get_fresh_var : ?name:string -> unit -> Ast.Coeus.Identifier.t

val get_fresh_proc : ?name:string -> unit -> Ast.Coeus.Identifier.t

val reset : unit -> unit
