open Core
open Ast.Ecoeus

let process_post post_cond =
  let open Postcondition in
  match post_cond.bare with
  | Assert {exprs; _} | Predicate {args= exprs; _} ->
      VarBinding.Set.of_list (Expr.free_vars_of_exprs exprs)

let run_pre (live_vars, acc) pre_cond =
  let open Precondition in
  let open Precondition in
  match pre_cond.bare with
  | Assume exprs | Predicate {args= exprs; _} ->
      let vars = Expr.free_vars_of_exprs exprs in
      let live_vars = List.fold vars ~init:live_vars ~f:VarBinding.Set.add in
      let acc = pre_cond :: acc in
      (live_vars, acc)
  | Assign (lhs, rhs) -> (
    match lhs.indices with
    | [] ->
        if not (VarBinding.Set.mem live_vars lhs.base) then
          (* Lhs of the assignment is dead. Remove it *)
          (live_vars, acc)
        else
          let live_vars = VarBinding.Set.remove live_vars lhs.base in
          let vars = Expr.free_vars_of rhs in
          let live_vars =
            List.fold vars ~init:live_vars ~f:VarBinding.Set.add
          in
          let acc = pre_cond :: acc in
          (live_vars, acc)
    | _ ->
        let msg =
          Fmt.strf
            "DeadAssignEliminator cannot handle array-based lvalues. Please \
             run LvalueSimplifier first"
        in
        failwith msg )
  | Havoc vs ->
      let vs' = List.filter vs ~f:(VarBinding.Set.mem live_vars) in
      let havoc' =
        if List.length vs = List.length vs' then pre_cond else mk_havoc vs'
      in
      let live_vars = List.fold vs' ~init:live_vars ~f:VarBinding.Set.remove in
      let acc = havoc' :: acc in
      (live_vars, acc)

let run_pre_post pre_conds post_cond =
  let init_live = process_post post_cond in
  let _, rev_pre_conds =
    List.fold pre_conds ~init:(init_live, []) ~f:(fun acc pre_cond ->
        run_pre acc pre_cond )
  in
  List.rev rev_pre_conds

let run_vc VerifCondition.({pre_conds; post_cond; blame_step; _} as vc) =
  let pre_conds' = run_pre_post pre_conds post_cond in
  if List.length pre_conds' = List.length pre_conds then vc
  else VerifCondition.mk ?blame_step pre_conds' post_cond

let run VerifState.({verif_conds; pred_env; _}) =
  let verif_conds = List.map verif_conds ~f:run_vc in
  let vs = VerifState.mk pred_env verif_conds in
  vs
