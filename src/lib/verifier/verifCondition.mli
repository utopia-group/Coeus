type t = private
  { pre_conds: Precondition.t list
        (** Preconditions are stored in reverse order *)
  ; post_cond: Postcondition.t
  ; blame_step: int option
  ; ast_size: int }
[@@deriving sexp, compare, hash]

val mk : ?blame_step:int -> Precondition.t list -> Postcondition.t -> t

val pp : t Fmt.t

val ast_size_of : t -> int

val free_vars_of : t -> Ast.Ecoeus.VarBinding.t list

val preds_of : t -> Ast.Ecoeus.Identifier.t list

val preds_of_vcs : t list -> Ast.Ecoeus.Identifier.t list
