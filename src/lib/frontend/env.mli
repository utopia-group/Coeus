open Ast.Coeus

(** Env represent the result of Resolver. It stores the metadata for each variable and for each procedure/function. **)
module VariableKind : sig
  type t = InParam | OutParam | Local | Quantified
  [@@deriving sexp, compare, hash]
end

module VariableInfo : sig
  type t = {ty: Type.t; kind: VariableKind.t; parent: Identifier.t option}
  [@@deriving sexp, compare, hash]
end

module VariableMap : sig
  type t = VariableInfo.t Identifier.Map.t [@@deriving sexp, compare]
end

module DeclMap : sig
  type t = FunDecl.t Identifier.Map.t [@@deriving sexp, compare]
end

module ProcInfo : sig
  type t = {proc_sig: ProcSignature.t; entry_side: Side.t option}
  [@@deriving sexp, compare]
end

module ProcMap : sig
  type t = ProcInfo.t Identifier.Map.t [@@deriving sexp, compare]
end

type t = {var_map: VariableMap.t; proc_map: ProcMap.t; decl_map: DeclMap.t}
[@@deriving sexp, compare]

val empty : t

val of_var_map : VariableMap.t -> t

val of_decl_map : DeclMap.t -> t
