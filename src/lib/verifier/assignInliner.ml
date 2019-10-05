open Core
open Ast.Ecoeus

let run_pre (env, acc) pre_cond =
  let open Precondition in
  let subst_f = Identifier.Map.find env in
  match pre_cond.bare with
  | Havoc vs ->
      let env =
        List.fold vs ~init:env ~f:(fun acc vb ->
            Identifier.Map.remove acc vb.name )
      in
      let acc = pre_cond :: acc in
      (env, acc)
  | Assume es ->
      let es' = List.map es ~f:(Expr.subst ~f:subst_f) in
      let assume = mk_assume es' in
      let acc = assume :: acc in
      (env, acc)
  | Predicate {name; args} ->
      let args' = List.map args ~f:(Expr.subst ~f:subst_f) in
      let pred = mk_predicate name args' in
      let acc = pred :: acc in
      (env, acc)
  | Assign (lhs, rhs) ->
    match lhs.indices with
    | [] ->
        let rhs = Expr.subst ~f:subst_f rhs in
        let env = Identifier.Map.set env ~key:lhs.base.name ~data:rhs in
        (env, acc)
    | _ ->
        let msg =
          Fmt.strf
            "AssignInliner cannot handle array-based lvalues. Please run \
             LvalueSimplifier first"
        in
        failwith msg

let run_post env post_cond =
  let open Postcondition in
  let subst_f = Identifier.Map.find env in
  match post_cond.bare with
  | Assert {exprs; is_final} ->
      let exprs = List.map exprs ~f:(Expr.subst ~f:subst_f) in
      mk_assert ~is_final exprs
  | Predicate {name; args} ->
      let args = List.map args ~f:(Expr.subst ~f:subst_f) in
      mk_predicate name args

let run_vc VerifCondition.({pre_conds; post_cond; blame_step; _}) =
  let env = Identifier.Map.empty in
  let env, pre_conds =
    List.fold (List.rev pre_conds) ~init:(env, []) ~f:run_pre
  in
  let post_cond = run_post env post_cond in
  VerifCondition.mk ?blame_step pre_conds post_cond

let run VerifState.({verif_conds; pred_env; _}) =
  let verif_conds = List.map verif_conds ~f:run_vc in
  let vs = VerifState.mk pred_env verif_conds in
  vs
