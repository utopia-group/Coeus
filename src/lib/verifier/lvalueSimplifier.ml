open Core
open Ast.Ecoeus

let run_pre pre =
  let open Precondition in
  match pre.bare with
  | Assign (Lvalue.({base; indices; _}), rhs) when not (List.is_empty indices) ->
      let lhs' = Lvalue.mk_var base in
      let rhs' = Expr.mk_array_store (Expr.mk_var base) indices rhs in
      mk_assign lhs' rhs'
  | _ -> pre

let run_vc VerifCondition.({pre_conds; post_cond; blame_step; _}) =
  let pre_conds = List.map pre_conds ~f:run_pre in
  VerifCondition.mk ?blame_step pre_conds post_cond

let run VerifState.({verif_conds; pred_env; _}) =
  let verif_conds = List.map verif_conds ~f:run_vc in
  let vs = VerifState.mk pred_env verif_conds in
  vs
