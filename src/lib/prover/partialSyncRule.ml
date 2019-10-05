open Core
open Ast.Ecoeus
open Verifier
open Frontend

let is_applicable _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | s0 :: _, s1 :: _ -> Stmt.is_loop s0 && Stmt.is_loop s1
  | _ -> false

let decompose_var_bindings bindings =
  List.unzip (List.map bindings ~f:(fun VarBinding.({name; ty}) -> (name, ty)))

let write_vars_of left_body right_body =
  let left_varset = Stmts.write_var_set_of left_body in
  let right_varset = Stmts.write_var_set_of right_body in
  Hash_set.iter right_varset ~f:(fun v -> Hash_set.add left_varset v) ;
  Hash_set.to_list left_varset

let apply_left_while left_cond left_body left_stmts right_cond right_body
    right_stmts ast env depth (goal: Goal.t) =
  (* Create the invariant predicate *)
  let left_proc = lookup_proc_exn ast goal.left_proc in
  let right_proc = lookup_proc_exn ast goal.right_proc in
  let var_bindings =
    List.concat
      [ left_proc.params
      ; left_proc.rets
      ; left_proc.locals
      ; right_proc.params
      ; right_proc.rets
      ; right_proc.locals ]
  in
  let pred_name = FreshNameGenerator.get_fresh_inv ~rel:true () in
  let inv_pred = PredSignature.{name= pred_name; params= var_bindings} in
  (* Current precondition implies the invariant *)
  let pred_args = List.map var_bindings ~f:(fun vb -> Expr.mk_var vb) in
  let pred_post = Postcondition.mk_predicate inv_pred.name pred_args in
  let pre_goal =
    Goal.replace goal ~left_stmts:[] ~right_stmts:[] ~post_cond:pred_post
  in
  (* Invariant is inductive *)
  let forall_pre = Precondition.mk_havoc var_bindings in
  let pred_pre = Precondition.mk_predicate inv_pred.name pred_args in
  let loop_stay_pre = Precondition.mk_assume [left_cond; right_cond] in
  let inv_pres = [pred_pre; loop_stay_pre; forall_pre] in
  let inv_goal =
    Goal.replace goal ~left_stmts:left_body ~right_stmts:right_body
      ~pre_conds:inv_pres ~post_cond:pred_post
  in
  (* When the sync-ed loop ends, one side must terminate *)
  let neg_left_cond = Expr.logical_negate_of left_cond in
  let neg_right_cond = Expr.logical_negate_of right_cond in
  let loop_stop_pre = Precondition.mk_assume [left_cond; neg_right_cond] in
  let stop_pres = [pred_pre; loop_stop_pre; forall_pre] in
  let stop_post =
    Postcondition.mk_assert ~is_final:false
      [Expr.mk_literal (Literal.BoolLit false)]
  in
  let stop_goal =
    Goal.replace goal ~left_stmts:[] ~right_stmts:[] ~pre_conds:stop_pres
      ~post_cond:stop_post
  in
  (* Stop goal is an assumption that this rule makes *)
  let stop_goal = Goal.assign_blame (Some depth) stop_goal in
  (* Invalidate info of the havoc-ed vars *)
  let write_vars = write_vars_of left_body right_body in
  let forall_pre = Precondition.mk_havoc write_vars in
  let loop_exit_pre = Precondition.mk_assume [neg_left_cond] in
  let post_pres = loop_exit_pre :: pred_pre :: forall_pre :: goal.pre_conds in
  (* Proceed with the invariant *)
  let post_left_stmt = Stmt.mk_while left_proc.name left_cond left_body in
  let post_right_stmt = Stmt.mk_while right_proc.name right_cond right_body in
  let post_right_stmts = post_right_stmt :: right_stmts in
  let post_goal =
    Goal.replace goal ~left_stmts ~right_stmts:post_right_stmts
      ~pre_conds:post_pres
  in
  let source = PredEnv.Source.RelLoopInv (post_left_stmt, post_right_stmt) in
  let env = PredEnv.extend env inv_pred ~source in
  Some ([pre_goal; stop_goal; inv_goal; post_goal], env)

let rec apply_left_impl left_stmt right_stmt left_stmts right_stmts ast env
    depth (goal: Goal.t) =
  match (left_stmt, right_stmt) with
  | ( Stmt.({bare_stmt= While {cond= left_cond; body= left_body}; _})
    , Stmt.({bare_stmt= While {cond= right_cond; body= right_body}; _}) ) ->
      apply_left_while left_cond left_body left_stmts right_cond right_body
        right_stmts ast env depth goal
  | ( Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _}
            as s0)
    , s1 )
    when Stmt.is_loop s1 ->
      let parent = Stmt.parent_of s0 in
      let lhs, rhs, cond, body =
        Stmt.while_of_for parent counter lower upper step direction body
      in
      let pre = Precondition.mk_assign lhs rhs in
      let pre_conds = pre :: goal.pre_conds in
      let while_stmt = Stmt.mk_while parent cond body in
      let goal' = Goal.replace goal ~pre_conds in
      apply_left_impl while_stmt right_stmt left_stmts right_stmts ast env
        depth goal'
  | ( Stmt.({bare_stmt= While _; _})
    , ( Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _})
      as s1 ) ) ->
      let parent = Stmt.parent_of s1 in
      let lhs, rhs, cond, body =
        Stmt.while_of_for parent counter lower upper step direction body
      in
      let pre = Precondition.mk_assign lhs rhs in
      let pre_conds = pre :: goal.pre_conds in
      let while_stmt = Stmt.mk_while parent cond body in
      let goal' = Goal.replace goal ~pre_conds in
      apply_left_impl left_stmt while_stmt left_stmts right_stmts ast env depth
        goal'
  | _ -> None

let apply_left ast env depth (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | left_stmt :: left_stmts, right_stmt :: right_stmts ->
      apply_left_impl left_stmt right_stmt left_stmts right_stmts ast env depth
        goal
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_semilocal_rule ~apply_left side in
  SemiLocalRule.mk_rule ~is_applicable ~is_aggressive:true ~name apply

let psync_l = create "psync_l" Side.Left

let psync_r = create "psync_r" Side.Right
