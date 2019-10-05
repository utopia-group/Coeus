type t = private
  { pre_conds: Verifier.Precondition.t list
  ; post_cond: Verifier.Postcondition.t
  ; left_proc: Ast.Ecoeus.Identifier.t
  ; left_stmts: Ast.Ecoeus.Stmt.t list
  ; right_proc: Ast.Ecoeus.Identifier.t
  ; right_stmts: Ast.Ecoeus.Stmt.t list
  ; blame_step: int option
  ; prev_step: int option
  ; subgoal_index: int option
  ; ast_size: int }

val mk :
     ?blame_step:int
  -> ?prev_step:int
  -> ?subgoal_index:int
  -> Verifier.Precondition.t list
  -> Verifier.Postcondition.t
  -> Ast.Ecoeus.Identifier.t
  -> Ast.Ecoeus.Stmt.t list
  -> Ast.Ecoeus.Identifier.t
  -> Ast.Ecoeus.Stmt.t list
  -> t

val replace :
     ?pre_conds:Verifier.Precondition.t list
  -> ?post_cond:Verifier.Postcondition.t
  -> ?left_proc:Ast.Ecoeus.Identifier.t
  -> ?left_stmts:Ast.Ecoeus.Stmt.t list
  -> ?right_proc:Ast.Ecoeus.Identifier.t
  -> ?right_stmts:Ast.Ecoeus.Stmt.t list
  -> t
  -> t

val assign_blame : int option -> t -> t

val set_prev : int option -> t -> t

val set_subgoal_index : int -> t -> t

val pp : t Fmt.t

val ast_size_of : t -> int

val stmt_count : t -> int

val swap_stmts : t -> t

val is_empty : t -> bool
(** Return true iff all stmts are turned into VCs *)

val extract_vc : t -> Verifier.VerifCondition.t
