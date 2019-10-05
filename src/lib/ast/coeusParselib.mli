val parse_string : string -> (Coeus.t, string) Core.Result.t
(** Parse texts supplied directly by the given string *)

val parse_file : string -> (Coeus.t, string) Core.Result.t
(** Parse texts read from a file with given name *)
