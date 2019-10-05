open Core
open Ast.Coeus

module VariableKind = struct
  type t = InParam | OutParam | Local | Quantified
  [@@deriving sexp, compare, hash]
end

module VariableInfo = struct
  type t = {ty: Type.t; kind: VariableKind.t; parent: Identifier.t option}
  [@@deriving sexp, compare, hash]
end

module VariableMap = struct
  type t = VariableInfo.t Identifier.Map.t [@@deriving sexp, compare]
end

module DeclMap = struct
  type t = FunDecl.t Identifier.Map.t [@@deriving sexp, compare]
end

module ProcInfo = struct
  type t = {proc_sig: ProcSignature.t; entry_side: Side.t option}
  [@@deriving sexp, compare]
end

module ProcMap = struct
  type t = ProcInfo.t Identifier.Map.t [@@deriving sexp, compare]
end

type t = {var_map: VariableMap.t; proc_map: ProcMap.t; decl_map: DeclMap.t}
[@@deriving sexp, compare]

let empty =
  { var_map= Identifier.Map.empty
  ; proc_map= Identifier.Map.empty
  ; decl_map= Identifier.Map.empty }

let of_var_map var_map =
  {var_map; proc_map= Identifier.Map.empty; decl_map= Identifier.Map.empty}

let of_decl_map decl_map =
  {var_map= Identifier.Map.empty; proc_map= Identifier.Map.empty; decl_map}
