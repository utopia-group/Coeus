open Core
open Verifier
open Ast.Ecoeus

let decompose_var_bindings bindings =
  List.unzip (List.map bindings ~f:(fun VarBinding.({name; ty}) -> (name, ty)))

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  not (List.is_empty stmts)

let advance_left (goal: Goal.t) pre_cond left_stmts =
  let pre_conds = pre_cond :: goal.pre_conds in
  Goal.replace goal ~pre_conds ~left_stmts

let apply_left_while env (goal: Goal.t) ast cond body left_stmts =
  (* Create the invariant predicate *)
  (* Note that the loop invariant may depend on variables outside of the loop's read/write var set. *)
  let left_proc = Ast.Ecoeus.lookup_proc_exn ast goal.left_proc in
  let right_proc = Ast.Ecoeus.lookup_proc_exn ast goal.right_proc in
  let var_bindings =
    List.concat
      [ left_proc.params
      ; left_proc.rets
      ; left_proc.locals
      ; right_proc.params
      ; right_proc.rets
      ; right_proc.locals ]
    |> List.sort ~compare:[%compare : VarBinding.t]
    |> List.remove_consecutive_duplicates
         ~equal:[%compare.equal : VarBinding.t]
  in
  let pred_name = FreshNameGenerator.get_fresh_inv () in
  let inv_pred = PredSignature.{name= pred_name; params= var_bindings} in
  (* Current precondition implies the invariant *)
  let pred_args = List.map var_bindings ~f:(fun v -> Expr.mk_var v) in
  let pred_post = Postcondition.mk_predicate inv_pred.name pred_args in
  let pre_goal =
    Goal.replace goal ~left_stmts:[] ~right_stmts:[] ~post_cond:pred_post
  in
  (* Invariant is inductive *)
  let forall_pre = Precondition.mk_havoc var_bindings in
  let pred_pre = Precondition.mk_predicate inv_pred.name pred_args in
  let loop_stay_pre = Precondition.mk_assume [cond] in
  let inv_pres = [pred_pre; loop_stay_pre; forall_pre] in
  let inv_goal =
    Goal.replace goal ~left_stmts:body ~right_stmts:[] ~pre_conds:inv_pres
      ~post_cond:pred_post
  in
  (* Invalidate info of the havoc-ed vars *)
  let write_vars = Hash_set.to_list (Stmts.write_var_set_of body) in
  let forall_pre = Precondition.mk_havoc write_vars in
  let loop_exit_pre = Precondition.mk_assume [Expr.logical_negate_of cond] in
  let post_pres = loop_exit_pre :: pred_pre :: forall_pre :: goal.pre_conds in
  (* Proceed with the invariant *)
  let post_goal = Goal.replace goal ~left_stmts ~pre_conds:post_pres in
  let source =
    PredEnv.Source.LoopInv (Stmt.mk_while goal.left_proc cond body)
  in
  let env = PredEnv.extend env inv_pred ~source in
  Some ([pre_goal; inv_goal; post_goal], env)

let lookup_summary env name =
  let pre_name = PredSignature.summary_pre_id name in
  let post_name = PredSignature.summary_post_id name in
  match (PredEnv.lookup env pre_name, PredEnv.lookup env post_name) with
  | ( Some PredEnv.Info.({signature= pre_sig; _})
    , Some PredEnv.Info.({signature= post_sig; _}) ) ->
      Some (pre_sig, post_sig)
  | _ -> None

let create_summary env (proc: Procedure.t) =
  let pre_name = PredSignature.summary_pre_id proc.name in
  let post_name = PredSignature.summary_post_id proc.name in
  let pre_sig = PredSignature.{name= pre_name; params= proc.params} in
  let post_sig =
    PredSignature.{name= post_name; params= List.append proc.rets proc.params}
  in
  let pre_src =
    PredEnv.(Source.ProcSummary (SummaryType.Precondition, proc))
  in
  let post_src =
    PredEnv.(Source.ProcSummary (SummaryType.Postcondition, proc))
  in
  let env = PredEnv.extend ~source:pre_src env pre_sig in
  let env = PredEnv.extend ~source:post_src env post_sig in
  (env, pre_sig, post_sig)

let apply_left_call env (goal: Goal.t) ast rets target args left_stmts =
  let target_proc = lookup_proc_exn ast target in
  let do_apply pre_pred_name post_pred_name =
    (* Assert the precondition of the function *)
    let pre_assert = Postcondition.mk_predicate pre_pred_name args in
    let pre_goal =
      Goal.replace goal ~post_cond:pre_assert ~left_stmts:[] ~right_stmts:[]
    in
    (* Assume the postcondition of the function *)
    let tmp_retvars =
      List.map target_proc.rets ~f:(fun vb ->
          let name = FreshNameGenerator.get_fresh_var ~name:"$tmpret" () in
          {vb with name} )
    in
    let post_havoc = Precondition.mk_havoc tmp_retvars in
    let tmp_retexprs = List.map tmp_retvars ~f:Expr.mk_var in
    let post_assume_args = List.append tmp_retexprs args in
    let post_assume =
      Precondition.mk_predicate post_pred_name post_assume_args
    in
    let post_assigns =
      List.map (List.zip_exn rets tmp_retexprs) ~f:(fun (lval, rval) ->
          Precondition.mk_assign lval rval )
    in
    let pre_conds =
      List.rev_append (post_havoc :: post_assume :: post_assigns)
        goal.pre_conds
    in
    let post_goal = Goal.replace goal ~pre_conds ~left_stmts in
    (pre_goal, post_goal)
  in
  match lookup_summary env target with
  | None ->
      let env, pre_pred, post_pred = create_summary env target_proc in
      let target_bindings =
        List.concat [target_proc.params; target_proc.rets; target_proc.locals]
      in
      let body_havoc = Precondition.mk_havoc target_bindings in
      let body_pre =
        Precondition.mk_predicate pre_pred.name
          (List.map target_proc.params ~f:Expr.mk_var)
      in
      (* It is VERY important that in the postcondition, we should use a COPY of the parameters instead of the parameters themselves directly. *)
      (* If we don't do that, the proof system becomes unsound when the parameter is modified in the function body *)
      let param_copies =
        List.map target_proc.params ~f:(fun vb ->
            let name = FreshNameGenerator.get_fresh_var ~name:"tmpparam" () in
            VarBinding.{vb with name} )
      in
      let param_copy_pres =
        List.map (List.zip_exn target_proc.params param_copies) ~f:
          (fun (param, copy) ->
            Precondition.mk_assign (Lvalue.mk_var copy) (Expr.mk_var param) )
      in
      let body_post =
        let post_args =
          List.map (List.append target_proc.rets param_copies) ~f:Expr.mk_var
        in
        Postcondition.mk_predicate post_pred.name post_args
      in
      let body_goal =
        Goal.mk
          (List.rev_append param_copy_pres [body_pre; body_havoc])
          body_post target_proc.name target_proc.stmts goal.right_proc []
      in
      let pre_goal, post_goal = do_apply pre_pred.name post_pred.name in
      Some ([pre_goal; body_goal; post_goal], env)
  | Some (pre_pred, post_pred) ->
      let pre_goal, post_goal = do_apply pre_pred.name post_pred.name in
      Some ([pre_goal; post_goal], env)

let apply_left ast env _ (goal: Goal.t) =
  match goal.left_stmts with
  | [] -> None
  | stmt :: left_stmts ->
      let open Stmt in
      match stmt.bare_stmt with
      | Assume e ->
          let pre = Precondition.mk_assume [e] in
          let goal' = advance_left goal pre left_stmts in
          Some ([goal'], env)
      | Assign {lhs; rhs} ->
          let pre = Precondition.mk_assign lhs rhs in
          let goal' = advance_left goal pre left_stmts in
          Some ([goal'], env)
      | If {cond; then_branch; else_branch} ->
          let neg_cond = Expr.logical_negate_of cond in
          let pos_pre = Precondition.mk_assume [cond] in
          let neg_pre = Precondition.mk_assume [neg_cond] in
          let pos_left = List.append then_branch left_stmts in
          let neg_left = List.append else_branch left_stmts in
          let pos_goal = advance_left goal pos_pre pos_left in
          let neg_goal = advance_left goal neg_pre neg_left in
          Some ([pos_goal; neg_goal], env)
      | For {counter; lower; upper; step; direction; body} ->
          let lhs, rhs, cond, body =
            Stmt.while_of_for (Stmt.parent_of stmt) counter lower upper step
              direction body
          in
          let pre = Precondition.mk_assign lhs rhs in
          let pre_conds = pre :: goal.pre_conds in
          let goal' = Goal.replace goal ~pre_conds in
          apply_left_while env goal' ast cond body left_stmts
      | While {cond; body} ->
          apply_left_while env goal ast cond body left_stmts
      | Call {rets; name; args} ->
          apply_left_call env goal ast rets name args left_stmts

let create name side =
  let apply = RuleHelper.mk_symmetric_semilocal_rule ~apply_left side in
  SemiLocalRule.mk_rule ~is_applicable:(is_applicable side) ~name apply

let seq_l = create "seq_l" Side.Left

let seq_r = create "seq_r" Side.Right
