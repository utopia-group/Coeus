type t = private
  {pred_env: PredEnv.t; verif_conds: VerifCondition.t list; ast_size: int}
[@@deriving sexp, compare]

val empty : t

val mk : PredEnv.t -> VerifCondition.t list -> t

val with_env : t -> PredEnv.t -> t

val pp : t Fmt.t

val ast_size_of : t -> int

val add_vc : t -> VerifCondition.t -> t
(** Convenient helper to add a vc to the state *)

val add_decl : ?source:PredEnv.Source.t -> t -> PredSignature.t -> t
(** Convenient helper to add a predicate declaration to the state *)

val validate : t -> (unit, string) Core.Result.t
(** Return an error iff the given state contains type inconsistencies and undefined variables *)
