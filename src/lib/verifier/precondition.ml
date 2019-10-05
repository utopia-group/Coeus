open Core
open Ast.Ecoeus

type t = {bare: u; ast_size: int} [@@deriving sexp, compare, hash]

and u =
  | Havoc of VarBinding.t list
  | Assume of Expr.t list
  | Assign of Lvalue.t * Expr.t
  | Predicate of {name: Identifier.t; args: Expr.t list}
[@@deriving sexp, compare, hash]

let mk_havoc vars =
  let ast_size = 1 + List.length vars in
  {bare= Havoc vars; ast_size}

let mk_assume es =
  let ast_size = 1 + Expr.ast_size_of_exprs es in
  {bare= Assume es; ast_size}

let mk_assign lhs rhs =
  let ast_size = 1 + Lvalue.ast_size_of lhs + Expr.ast_size_of rhs in
  {bare= Assign (lhs, rhs); ast_size}

let mk_predicate name args =
  let ast_size = 1 + Expr.ast_size_of_exprs args in
  {bare= Predicate {name; args}; ast_size}

let ast_size_of {ast_size; _} = ast_size

let free_vars_of {bare; _} =
  match bare with
  | Assume es -> Expr.free_vars_of_exprs es
  | Predicate {args; _} -> Expr.free_vars_of_exprs args
  | Assign (lhs, rhs) ->
      let lhs_set = Expr.free_var_set_of_exprs lhs.indices in
      let rhs_set = Expr.free_var_set_of rhs in
      Hash_set.iter lhs_set ~f:(Hash_set.add rhs_set) ;
      Hash_set.remove rhs_set lhs.base ;
      Hash_set.to_list rhs_set
  | Havoc _ -> []

let pp fmt c =
  match c.bare with
  | Havoc vs ->
      let pp_var fmt VarBinding.({name; _}) =
        Fmt.pf fmt "%a" Identifier.pp name
      in
      Fmt.pf fmt "havoc %a" (Fmt.list ~sep:Fmt.comma pp_var) vs
  | Assume es ->
      let pp_exprs =
        let and_sep fmt () = Fmt.pf fmt " && " in
        Fmt.list ~sep:and_sep Expr.pp
      in
      Fmt.pf fmt "assume %a" pp_exprs es
  | Assign (lhs, rhs) -> Fmt.pf fmt "let (%a = %a)" Lvalue.pp lhs Expr.pp rhs
  | Predicate {name; args} ->
      Fmt.pf fmt "assume %a(%a)" Identifier.pp name
        (Fmt.list ~sep:Fmt.comma Expr.pp)
        args
