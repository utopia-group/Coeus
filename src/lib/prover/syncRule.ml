open Core
open Ast.Ecoeus
open Verifier
open Frontend

module SyncSide = struct
  type t = Neither | Both | Left | Right
end

let decompose_var_bindings bindings =
  List.unzip (List.map bindings ~f:(fun VarBinding.({name; ty}) -> (name, ty)))

let write_vars_of left_body right_body =
  let left_varset = Stmts.write_var_set_of left_body in
  let right_varset = Stmts.write_var_set_of right_body in
  Hash_set.iter right_varset ~f:(fun v -> Hash_set.add left_varset v) ;
  Hash_set.to_list left_varset

let is_applicable _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | s0 :: _, s1 :: _ -> Stmt.is_loop s0 && Stmt.is_loop s1
  | _ -> false

let apply_whiles sync_side ast env depth (goal: Goal.t) left_cond left_body
    left_stmts right_cond right_body right_stmts =
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
  let pre_goal =
    match sync_side with
    | SyncSide.Neither -> pre_goal
    | _ -> Goal.assign_blame (Some depth) pre_goal
  in
  (* Invariant is inductive for synced iterations *)
  let forall_pre = Precondition.mk_havoc var_bindings in
  let pred_pre = Precondition.mk_predicate inv_pred.name pred_args in
  let loop_stay_pre = Precondition.mk_assume [left_cond; right_cond] in
  let inv_pres = [pred_pre; loop_stay_pre; forall_pre] in
  let inv_goal =
    Goal.replace goal ~left_stmts:left_body ~right_stmts:right_body
      ~pre_conds:inv_pres ~post_cond:pred_post
  in
  (* Invariant is inductive for left-only iterations *)
  let neg_left_cond = Expr.logical_negate_of left_cond in
  let neg_right_cond = Expr.logical_negate_of right_cond in
  let left_loop_stay_pre =
    Precondition.mk_assume [left_cond; neg_right_cond]
  in
  let left_inv_pres = [pred_pre; left_loop_stay_pre; forall_pre] in
  let left_inv_stmts, left_inv_post, can_blame =
    match sync_side with
    | SyncSide.Left | SyncSide.Both ->
        ( []
        , Postcondition.mk_assert ~is_final:false
            [Expr.mk_literal (Literal.BoolLit false)]
        , true )
    | _ -> (left_body, pred_post, false)
  in
  let left_inv_goal =
    Goal.replace goal ~left_stmts:left_inv_stmts ~right_stmts:[]
      ~pre_conds:left_inv_pres ~post_cond:left_inv_post
  in
  let left_inv_goal =
    if can_blame then Goal.assign_blame (Some depth) left_inv_goal
    else left_inv_goal
  in
  (* Invariant is inductive for right-only iterations *)
  let right_loop_stay_pre =
    Precondition.mk_assume [neg_left_cond; right_cond]
  in
  let right_inv_pres = [pred_pre; right_loop_stay_pre; forall_pre] in
  let right_inv_stmts, right_inv_post, can_blame =
    match sync_side with
    | SyncSide.Right | SyncSide.Both ->
        ( []
        , Postcondition.mk_assert ~is_final:false
            [Expr.mk_literal (Literal.BoolLit false)]
        , true )
    | _ -> (right_body, pred_post, false)
  in
  let right_inv_goal =
    Goal.replace goal ~left_stmts:[] ~right_stmts:right_inv_stmts
      ~pre_conds:right_inv_pres ~post_cond:right_inv_post
  in
  let right_inv_goal =
    if can_blame then Goal.assign_blame (Some depth) right_inv_goal
    else right_inv_goal
  in
  (* Invalidate info of the havoc-ed vars *)
  let write_vars = write_vars_of left_body right_body in
  let forall_pre = Precondition.mk_havoc write_vars in
  let loop_exit_pre = Precondition.mk_assume [neg_left_cond; neg_right_cond] in
  let post_pres = loop_exit_pre :: pred_pre :: forall_pre :: goal.pre_conds in
  (* Proceed with the invariant *)
  let post_goal =
    Goal.replace goal ~left_stmts ~right_stmts ~pre_conds:post_pres
  in
  let source =
    PredEnv.Source.RelLoopInv
      ( Stmt.mk_while goal.left_proc left_cond left_body
      , Stmt.mk_while goal.right_proc right_cond right_body )
  in
  let env = PredEnv.extend env inv_pred ~source in
  Some ([pre_goal; right_inv_goal; left_inv_goal; inv_goal; post_goal], env)

let rec apply sync_side ast env depth (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | ( Stmt.({bare_stmt= While {cond= left_cond; body= left_body}; _})
      :: left_stmts
    , Stmt.({bare_stmt= While {cond= right_cond; body= right_body}; _})
      :: right_stmts ) ->
      apply_whiles sync_side ast env depth goal left_cond left_body left_stmts
        right_cond right_body right_stmts
  | ( ( Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _})
      as s0 )
      :: left_stmts
    , s1 :: _ )
    when Stmt.is_loop s1 ->
      let parent = Stmt.parent_of s0 in
      let lhs, rhs, cond, body =
        Stmt.while_of_for parent counter lower upper step direction body
      in
      let pre = Precondition.mk_assign lhs rhs in
      let pre_conds = pre :: goal.pre_conds in
      let while_stmt = Stmt.mk_while parent cond body in
      let left_stmts = while_stmt :: left_stmts in
      let goal' = Goal.replace goal ~pre_conds ~left_stmts in
      apply sync_side ast env depth goal'
  | ( Stmt.({bare_stmt= While _; _}) :: _
    , ( Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _})
      as s1 )
      :: right_stmts ) ->
      let parent = Stmt.parent_of s1 in
      let lhs, rhs, cond, body =
        Stmt.while_of_for parent counter lower upper step direction body
      in
      let pre = Precondition.mk_assign lhs rhs in
      let pre_conds = pre :: goal.pre_conds in
      let while_stmt = Stmt.mk_while parent cond body in
      let right_stmts = while_stmt :: right_stmts in
      let goal' = Goal.replace goal ~pre_conds ~right_stmts in
      apply sync_side ast env depth goal'
  | _ -> None

let create name side =
  let is_aggressive =
    match side with SyncSide.Neither -> false | _ -> true
  in
  SemiLocalRule.mk_rule ~is_applicable ~is_aggressive ~name (apply side)

let sync = create "sync" SyncSide.Both

let sync_l = create "sync_l" SyncSide.Left

let sync_r = create "sync_r" SyncSide.Right

let sync_n = create "sync_n" SyncSide.Neither
