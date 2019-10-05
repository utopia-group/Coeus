(* The running time of Z3 spacer (annoyingly) depends on the variable names. *)
(* This pass renames all the variables in order to remove that level of dependency *)

val run : VerifState.t -> VerifState.t
