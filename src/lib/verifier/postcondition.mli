open Ast.Ecoeus

type t = private {bare: u; ast_size: int} [@@deriving sexp, compare, hash]

and u =
  | Assert of {exprs: Expr.t list; is_final: bool}
  | Predicate of {name: Identifier.t; args: Expr.t list}
[@@deriving sexp, compare, hash]

val mk_assert : ?is_final:bool -> Expr.t list -> t

val mk_predicate : Identifier.t -> Expr.t list -> t

val pp : t Fmt.t

val ast_size_of : t -> int

val free_vars_of : t -> VarBinding.t list
