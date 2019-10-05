(** This module defines AST types for ECoeus, the "elaborated" variant of the
    Coeus AST.  In this variant, each ast node is extended with other
    information such as type and subtree size.  The extension makes it easy for
    subsequent operations to obtain those information.

    Translating from Coeus AST to ECoeus AST requires variable resolution and
    type checking. This is not possible in the ast library and instead one
    should look for the corresponding APIs in the frontend library.

 *)

module Identifier = Coeus.Identifier
module Type = Coeus.Type
module Literal = Coeus.Literal
module Quantifier = Coeus.Quantifier
module UnaryOperator = Coeus.UnaryOperator
module BinaryOperator = Coeus.BinaryOperator
module Side = Coeus.Side
module VarBinding = Coeus.VarBinding
module FunDecl = Coeus.FunDecl
module EntrySpec = Coeus.EntrySpec

module ExprInfo : sig
  type t = {ast_size: int; ty: Type.t} [@@deriving sexp, compare, hash]

  val pp : t Fmt.t
end

module StmtInfo : sig
  type t = {ast_size: int; parent: Identifier.t}
  [@@deriving sexp, compare, hash]

  val pp : t Fmt.t
end

module Expr : sig
  type t = private {bare_expr: u; info: ExprInfo.t}
  [@@deriving sexp, compare, hash]

  and u =
    | LiteralExpr of Literal.t
    | VarExpr of VarBinding.t
    | UnaryExpr of UnaryOperator.t * t
    | BinaryExpr of BinaryOperator.t * t * t
    | ArraySelectExpr of {base: t; indices: t list}
    | ArrayStoreExpr of {base: t; indices: t list; value: t}
    | CondExpr of {cond: t; true_val: t; false_val: t}
    | FunCallExpr of {func: FunDecl.t; args: t list}
    | QuantifiedExpr of
        { quantifier: Quantifier.t
        ; bindings: VarBinding.t list
        ; body: t }
  [@@deriving sexp, compare, hash]

  val equal : t -> t -> bool

  val mk_literal : Literal.t -> t

  val mk_var : VarBinding.t -> t

  val mk_unary : UnaryOperator.t -> t -> t

  val mk_binary : BinaryOperator.t -> t -> t -> t

  val mk_array_select : t -> t list -> Type.t -> t

  val mk_array_store : t -> t list -> t -> t

  val mk_cond : t -> t -> t -> t

  val mk_funcall : FunDecl.t -> t list -> t

  val mk_quantified : Quantifier.t -> VarBinding.t list -> t -> t

  val logical_negate_of : t -> t

  val conjunct_exprs : t list -> t

  val subst : f:(Identifier.t -> t option) -> t -> t

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val iter_var : f:(VarBinding.t -> unit) -> t -> unit

  val free_var_set_of : t -> VarBinding.Hash_set.t

  val free_var_set_of_exprs : t list -> VarBinding.Hash_set.t

  val free_vars_of : t -> VarBinding.t list

  val free_vars_of_exprs : t list -> VarBinding.t list

  val literals_of : t -> Literal.t list

  val literals_of_exprs : t list -> Literal.t list

  val to_coeus : t -> Coeus.Expr.t

  val simplify : t -> t

  val type_of : t -> Type.t

  val ast_size_of : t -> int

  val ast_size_of_exprs : t list -> int

  val pp : t Fmt.t

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Lvalue : sig
  type t = private {base: VarBinding.t; indices: Expr.t list; info: ExprInfo.t}
  [@@deriving sexp, compare, hash]

  val mk_var : VarBinding.t -> t

  val mk_array : VarBinding.t -> Expr.t list -> Type.t -> t

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val iter_var : f:(VarBinding.t -> unit) -> t -> unit

  val map_expr : f:(Expr.t -> Expr.t) -> t -> t

  val iter_expr : f:(Expr.t -> unit) -> t -> unit

  val type_of : t -> Type.t

  val ast_size_of : t -> int

  val to_coeus : t -> Coeus.Lvalue.t

  val pp : t Fmt.t
end

module Stmt : sig
  module ForDirection = Coeus.Stmt.ForDirection

  type t = private {bare_stmt: u; info: StmtInfo.t}
  [@@deriving sexp, compare, hash]

  and u =
    | Assume of Expr.t
    | Assign of {lhs: Lvalue.t; rhs: Expr.t}
    | If of {cond: Expr.t; then_branch: t list; else_branch: t list}
    | While of {cond: Expr.t; body: t list}
    | For of
        { counter: Identifier.t
        ; lower: Expr.t
        ; upper: Expr.t
        ; step: Expr.t
        ; direction: ForDirection.t
        ; body: t list }
    | Call of {rets: Lvalue.t list; name: Identifier.t; args: Expr.t list}
  [@@deriving sexp, compare, hash]

  val mk_assume : Identifier.t -> Expr.t -> t

  val mk_assign : Identifier.t -> Lvalue.t -> Expr.t -> t

  val mk_if : Identifier.t -> Expr.t -> t list -> t list -> t

  val mk_while : Identifier.t -> Expr.t -> t list -> t

  val mk_for :
       Identifier.t
    -> Identifier.t
    -> Expr.t
    -> Expr.t
    -> Expr.t
    -> ForDirection.t
    -> t list
    -> t

  val mk_call :
    Identifier.t -> Lvalue.t list -> Identifier.t -> Expr.t list -> t

  val with_parent : t -> Identifier.t -> t

  val while_of_for :
       Identifier.t
    -> Identifier.t
    -> Expr.t
    -> Expr.t
    -> Expr.t
    -> ForDirection.t
    -> t list
    -> Lvalue.t * Expr.t * Expr.t * t list

  val ast_size_of : t -> int

  val parent_of : t -> Identifier.t

  val is_simple : t -> bool

  val is_loop : t -> bool

  val is_branch : t -> bool

  val is_call : t -> bool

  val count : t -> int

  val loop_nest_level_of : t -> int

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val iter_var : f:(VarBinding.t -> unit) -> t -> unit

  val map_expr : f:(Expr.t -> Expr.t) -> t -> t

  val iter_expr : f:(Expr.t -> unit) -> t -> unit

  val read_var_set_of : ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val write_var_set_of : ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val read_write_var_set_of :
    ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val conds_of : t -> Expr.t list

  val assign_rhs_of : t -> Expr.t list

  val to_coeus : t -> Coeus.Stmt.t

  val pp : t Fmt.t

  val pp_brief : t Fmt.t
end

module Stmts : sig
  type t = Stmt.t list [@@deriving sexp, compare, hash]

  val ast_size_of : t -> int

  val count : t -> int

  val loop_nest_level_of : t -> int

  val with_parent : t -> Identifier.t -> t

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val iter_var : f:(VarBinding.t -> unit) -> t -> unit

  val map_expr : f:(Expr.t -> Expr.t) -> t -> t

  val iter_expr : f:(Expr.t -> unit) -> t -> unit

  val read_var_set_of : ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val write_var_set_of : ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val read_write_var_set_of :
    ?exclude_counter:bool -> t -> VarBinding.Hash_set.t

  val conds_of : t -> Expr.t list

  val assign_rhs_of : t -> Expr.t list

  val to_coeus : t -> Coeus.Stmts.t

  val pp : t Fmt.t

  val pp_brief : t Fmt.t
end

module Procedure : sig
  type t =
    { name: Identifier.t
    ; params: VarBinding.t list
    ; rets: VarBinding.t list
    ; locals: VarBinding.t list
    ; stmts: Stmt.t list }
  [@@deriving sexp, compare, hash]

  val to_coeus : t -> Coeus.Procedure.t
end

module Spec : sig
  type t = {requires: Expr.t list; ensures: Expr.t list}
  [@@deriving sexp, compare, hash]

  val to_coeus : t -> Coeus.Spec.t
end

type t = {procs: Procedure.t list; entry: EntrySpec.t; spec: Spec.t}
[@@deriving sexp, compare, hash]

val to_coeus : t -> Coeus.t

val lookup_proc : t -> Identifier.t -> Procedure.t option

val lookup_proc_exn : t -> Identifier.t -> Procedure.t

val update_proc : t -> Procedure.t -> t
