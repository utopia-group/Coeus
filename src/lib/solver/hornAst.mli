open Core
open Ast.Ecoeus

module Predicate : sig
  type t = {name: Identifier.t; params: VarBinding.t list; hints: Expr.t list}
  [@@deriving sexp, compare, hash]

  val pp : t Fmt.t

  module Apply : sig
    type t = {name: Identifier.t; args: Expr.t list}
    [@@deriving sexp, compare, hash]

    val pp : t Fmt.t
  end
end

module Rule : sig
  module Head : sig
    type t = False | Predicate of Predicate.Apply.t
    [@@deriving sexp, compare, hash]

    val pp : t Fmt.t
  end

  module Body : sig
    type t = Expr of Expr.t | Predicate of Predicate.Apply.t
    [@@deriving sexp, compare, hash]

    val subst_predicate :
      f:(Identifier.t -> Expr.t list -> Expr.t) -> t -> Expr.t

    val predicates_of : t -> Identifier.t list

    val pp : t Fmt.t
  end

  module Bodies : sig
    type t = Body.t list [@@deriving sexp, compare, hash]

    val subst_predicate :
      f:(Identifier.t -> Expr.t list -> Expr.t) -> t -> Expr.t list

    val predicates_of : t -> Identifier.t list

    val pp : t Fmt.t
  end

  type t =
    { name: Identifier.t
    ; bindings: VarBinding.t list
    ; head: Head.t
    ; bodies: Body.t list }
  [@@deriving sexp, compare, hash]

  val predicates_of : t -> Identifier.t list

  val pp : t Fmt.t
end

type t

val pp : t Fmt.t

val create : Predicate.t list -> Rule.t list -> t

val pred_env_of : t -> Predicate.t Identifier.Map.t

val preds_of : t -> Predicate.t list

val num_preds : t -> int

val lookup_pred : t -> Identifier.t -> Predicate.t option

val lookup_pred_exn : t -> Identifier.t -> Predicate.t

val rules_of : t -> Rule.t list

val num_rules : t -> int

val lookup_rule : t -> Identifier.t -> Rule.t option

val lookup_rule_exn : t -> Identifier.t -> Rule.t

val rules_of_head : t -> Identifier.t -> Rule.t list
