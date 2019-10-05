module Identifier : sig
  type t [@@deriving sexp, compare, hash]

  val of_string : string -> t

  val string_of : t -> string

  val equal : t -> t -> bool

  val pp : t Fmt.t

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Type : sig
  type t = BoolType | IntType | ArrayType of t list * t
  [@@deriving sexp, compare, hash]

  val is_array_type : t -> bool

  val equal : t -> t -> bool

  val pp : t Fmt.t

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Literal : sig
  type t = BoolLit of bool | IntLit of Bigint.t
  [@@deriving sexp, compare, hash]

  val pp : t Fmt.t

  val type_of : t -> Type.t

  val of_bool : bool -> t

  val of_int : int -> t

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Quantifier : sig
  type t = Exists | ForAll [@@deriving sexp, compare, hash]

  val pp : t Fmt.t
end

module UnaryOperator : sig
  type t = Neg | Not [@@deriving sexp, compare, hash]

  val ret_type_of : t -> Type.t

  val op_type_of : t -> Type.t

  val pp : t Fmt.t
end

module BinaryOperator : sig
  type t =
    (* Arithmetic *)
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    (* Logical *)
    | And
    | Or
    | Imply
    (* Relational *)
    | Lt
    | Le
    | Gt
    | Ge
    | Eq
    | Ne
  [@@deriving sexp, compare, hash]

  val ret_type_of : t -> Type.t

  val op_type_of : t -> Type.t * Type.t

  val pp : t Fmt.t
end

module Side : sig
  type t = Left | Right [@@deriving sexp, compare, hash]

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module VarBinding : sig
  type t = {name: Identifier.t; ty: Type.t} [@@deriving sexp, compare, hash]

  val pp : t Fmt.t

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Expr : sig
  type t =
    | LiteralExpr of Literal.t
    | VarExpr of Identifier.t
    | UnaryExpr of UnaryOperator.t * t
    | BinaryExpr of BinaryOperator.t * t * t
    | ArraySelectExpr of {base: t; indices: t list}
    | ArrayStoreExpr of {base: t; indices: t list; value: t}
    | AnnotatedExpr of {annot: Side.t; expr: t}
    | CondExpr of {cond: t; true_val: t; false_val: t}
    | FunCallExpr of {name: Identifier.t; args: t list}
    | QuantifiedExpr of
        { quantifier: Quantifier.t
        ; bindings: VarBinding.t list
        ; body: t }
  [@@deriving sexp, compare, hash]

  val equal : t -> t -> bool

  val logical_negate_of : t -> t

  val simplify : t -> t

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val subst : t Identifier.Map.t -> t -> t

  val free_var_set_of : t -> Identifier.Hash_set.t

  val free_var_set_of_exprs : t list -> Identifier.Hash_set.t

  val free_vars_of : t -> Identifier.t list

  val quick_type_of :
       ?ret_env:Type.t Identifier.Map.t
    -> var_env:Type.t Identifier.Map.t
    -> t
    -> Type.t option
  (** Quickly determine the type of the given expression (with no full consistency check). *)

  val quick_type_of_exn :
       ?ret_env:Type.t Identifier.Map.t
    -> var_env:Type.t Identifier.Map.t
    -> t
    -> Type.t

  val pp : t Fmt.t
end

module Lvalue : sig
  type t = {base: Identifier.t; indices: Expr.t list}
  [@@deriving sexp, compare, hash]

  val to_expr : t -> Expr.t

  val of_var : Identifier.t -> t

  val quick_type_of : var_env:Type.t Identifier.Map.t -> t -> Type.t option
  (** Quickly determine the type of the given expression (with no full consistency check). *)

  val quick_type_of_exn : var_env:Type.t Identifier.Map.t -> t -> Type.t

  val pp : t Fmt.t
end

module Stmt : sig
  module ForDirection : sig
    type t = Forward | Backward [@@deriving sexp, compare, hash]

    val equal : t -> t -> bool
  end

  type t =
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

  val is_loop : t -> bool

  val is_branch : t -> bool

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val map_expr : f:(Expr.t -> Expr.t) -> t -> t

  val read_var_set_of : ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val write_var_set_of : ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val read_write_var_set_of :
    ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val pp : t Fmt.t

  val pp_brief : t Fmt.t
end

module Stmts : sig
  type t = Stmt.t list

  val map_var : f:(Identifier.t -> Identifier.t) -> t -> t

  val map_expr : f:(Expr.t -> Expr.t) -> t -> t

  val read_var_set_of : ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val write_var_set_of : ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val read_write_var_set_of :
    ?exclude_counter:bool -> t -> Identifier.Hash_set.t

  val pp : t Fmt.t

  val pp_brief : t Fmt.t
end

module FunDecl : sig
  type t = {name: Identifier.t; param_tys: Type.t list; ret_ty: Type.t}
  [@@deriving sexp, compare, hash]

  module Table : Core.Hashtbl.S with type key = t

  module Hash_set : Core.Hash_set.S with type elt = t

  module Set : Core.Set.S with type Elt.t = t

  module Map : Core.Map.S with type Key.t = t
end

module Procedure : sig
  type t =
    { name: Identifier.t
    ; params: VarBinding.t list
    ; rets: VarBinding.t list
    ; locals: VarBinding.t list
    ; stmts: Stmt.t list }
  [@@deriving sexp, compare, hash]
end

module EntrySpec : sig
  type t = {left: Identifier.t; right: Identifier.t}
  [@@deriving sexp, compare, hash]

  val default_left : Identifier.t

  val default_right : Identifier.t

  val default : t
end

module Spec : sig
  type t = {requires: Expr.t list; ensures: Expr.t list}
  [@@deriving sexp, compare, hash]
end

type t =
  { decls: FunDecl.t list
  ; procs: Procedure.t list
  ; entry: EntrySpec.t
  ; spec: Spec.t }
[@@deriving sexp, compare, hash]

val lookup_decl : t -> Identifier.t -> FunDecl.t option

val lookup_decl_exn : t -> Identifier.t -> FunDecl.t

val lookup_proc : t -> Identifier.t -> Procedure.t option

val lookup_proc_exn : t -> Identifier.t -> Procedure.t

val lookup_entry_proc : t -> Side.t -> Procedure.t option

val lookup_entry_proc_exn : t -> Side.t -> Procedure.t

val spec_map : t -> f:(Expr.t -> Expr.t) -> t

val proc_map : t -> f:(Procedure.t -> Procedure.t) -> t

val update_proc : t -> Procedure.t -> t
