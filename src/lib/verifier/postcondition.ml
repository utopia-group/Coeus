open Core
open Ast.Ecoeus

type t = {bare: u; ast_size: int} [@@deriving sexp, compare, hash]

and u =
  | Assert of {exprs: Expr.t list; is_final: bool}
  | Predicate of {name: Identifier.t; args: Expr.t list}
[@@deriving sexp, compare, hash]

let mk_assert ?(is_final= false) exprs =
  let ast_size = 1 + Expr.ast_size_of_exprs exprs in
  {bare= Assert {exprs; is_final}; ast_size}

let mk_predicate name args =
  let ast_size = 1 + Expr.ast_size_of_exprs args in
  {bare= Predicate {name; args}; ast_size}

let ast_size_of {ast_size; _} = ast_size

let free_vars_of {bare; _} =
  match bare with
  | Assert {exprs; _} -> Expr.free_vars_of_exprs exprs
  | Predicate {args; _} -> Expr.free_vars_of_exprs args

let pp fmt c =
  match c.bare with
  | Assert {exprs; _} ->
      let pp_exprs =
        let and_sep fmt () = Fmt.pf fmt " && " in
        Fmt.list ~sep:and_sep Expr.pp
      in
      Fmt.pf fmt "assert %a" pp_exprs exprs
  | Predicate {name; args} ->
      Fmt.pf fmt "assert %a(%a)" Identifier.pp name
        (Fmt.list ~sep:Fmt.comma Expr.pp)
        args
