(** Resolve all variable and function declarations in the program. *)

val run : Ast.Coeus.t -> (Env.t, string) Core.Result.t
