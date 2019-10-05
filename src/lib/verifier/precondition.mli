open Ast.Ecoeus

type t = private {bare: u; ast_size: int} [@@deriving sexp, compare, hash]

and u =
  | Havoc of VarBinding.t list
  | Assume of Expr.t list
  | Assign of Lvalue.t * Expr.t
  | Predicate of {name: Identifier.t; args: Expr.t list}
[@@deriving sexp, compare, hash]

val mk_havoc : VarBinding.t list -> t

val mk_assume : Expr.t list -> t

val mk_assign : Lvalue.t -> Expr.t -> t

val mk_predicate : Identifier.t -> Expr.t list -> t

val pp : t Fmt.t

val ast_size_of : t -> int

val free_vars_of : t -> VarBinding.t list
