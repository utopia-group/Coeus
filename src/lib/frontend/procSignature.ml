open Core
open Ast.Coeus

type t =
  {param_tys: Type.t list; ret_tys: Type.t list}
  [@@deriving (sexp, compare, hash)]

let of_proc Procedure.({params; rets; _}) =
  let type_of_binding (binding: VarBinding.t) = binding.ty in
  let param_tys = List.map params ~f:type_of_binding in
  let ret_tys = List.map rets ~f:type_of_binding in
  {param_tys; ret_tys}
