open Ast.Coeus

type t =
  {param_tys: Type.t list; ret_tys: Type.t list}
  [@@deriving (sexp, compare, hash)]

val of_proc : Procedure.t -> t
