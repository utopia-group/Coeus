open Ast.Ecoeus

type t = {name: Identifier.t; params: VarBinding.t list}
[@@deriving sexp, compare, hash]

val param_types_of : t -> Type.t list

val smtlib_of_t : t -> Core.Sexp.t

val t_of_smtlib : Core.Sexp.t -> t

val z3rel_of_t : t -> Core.Sexp.t

val t_of_z3rel : Core.Sexp.t -> t

val z3_query_id : Identifier.t

val z3_query_rel : t

val summary_pre_id : Identifier.t -> Identifier.t

val summary_post_id : Identifier.t -> Identifier.t

val mutual_summary_pre_id : Identifier.t -> Identifier.t -> Identifier.t

val mutual_summary_post_id : Identifier.t -> Identifier.t -> Identifier.t

val pp : t Fmt.t
