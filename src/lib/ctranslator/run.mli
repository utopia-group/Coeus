open Ast.Coeus

val init : unit -> unit
(** This function sets up the initial global state. Must be invoked before other APIs *)

val translate_one_file :
     TransConfig.t
  -> Side.t
  -> string
  -> (Procedure.t * Procedure.t list, string) Core.Result.t

val translate_pair :
  TransConfig.t -> string -> string -> (t, string) Core.Result.t
