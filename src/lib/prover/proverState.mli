type t =
  { ast: Ast.Ecoeus.t
  ; depth: int
  ; goals: Goal.t list
  ; verif_state: Verifier.VerifState.t }

val pp : t Fmt.t

val init : Ast.Ecoeus.t -> t

val num_goals : t -> int

val stmt_count : t -> int

val ast_size_of : t -> int

val is_empty : t -> bool
(** Return true iff all goals are empty *)

val is_fully_resolved : t -> bool
(** Return true iff all goals are resolved *)

val rotate_top : t -> t

val resolve : t -> t
(** Merge the VCs of empty goals into verif_state *)

val resolve_top : t -> t
(** Merge the VCs of empty goals into verif_state. More efficient than [resolve] but stops immediately after seeing the first nonempty goal from the goal stack *)

val current_goal_of : t -> Goal.t option
(** Get the currently active goal *)

val current_subgoal_index_of : t -> int option

val assign_subgoal_indices : t -> t

val check_limit : ProverConfig.t -> t -> (unit, string) Core.Result.t

val exceeds_limit : ProverConfig.t -> t -> bool
