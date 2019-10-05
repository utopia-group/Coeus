open Core
open Verifier
open Ast.Ecoeus

let is_applicable _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | Stmt.({bare_stmt= Call _; _}) :: _, Stmt.({bare_stmt= Call _; _}) :: _ ->
      true
  | _ -> false

let lookup_summary env (left_proc: Procedure.t) (right_proc: Procedure.t) =
  let pre_name =
    PredSignature.mutual_summary_pre_id left_proc.name right_proc.name
  in
  let post_name =
    PredSignature.mutual_summary_post_id left_proc.name right_proc.name
  in
  match (PredEnv.lookup env pre_name, PredEnv.lookup env post_name) with
  | ( Some PredEnv.Info.({signature= pre_sig; _})
    , Some PredEnv.Info.({signature= post_sig; _}) ) ->
      Some (pre_sig, post_sig)
  | _ -> None

let create_summary env (left_proc: Procedure.t) (right_proc: Procedure.t) =
  let pre_name =
    PredSignature.mutual_summary_pre_id left_proc.name right_proc.name
  in
  let post_name =
    PredSignature.mutual_summary_post_id left_proc.name right_proc.name
  in
  let params = List.append left_proc.params right_proc.params in
  let rets = List.append left_proc.rets right_proc.rets in
  let pre_sig = PredSignature.{name= pre_name; params} in
  let post_sig =
    PredSignature.{name= post_name; params= List.append rets params}
  in
  let pre_src =
    let open PredEnv in
    Source.RelProcSummary (SummaryType.Precondition, left_proc, right_proc)
  in
  let post_src =
    let open PredEnv in
    Source.RelProcSummary (SummaryType.Postcondition, left_proc, right_proc)
  in
  let env = PredEnv.extend ~source:pre_src env pre_sig in
  let env = PredEnv.extend ~source:post_src env post_sig in
  (env, pre_sig, post_sig)

let do_apply left_rets left_proc left_args left_stmts right_rets right_proc
    right_args right_stmts env goal =
  let args = List.append left_args right_args in
  let rets = List.append left_rets right_rets in
  let do_apply_impl pre_pred_name post_pred_name =
    (* Assert the mutual precondition *)
    let pre_assert = Postcondition.mk_predicate pre_pred_name args in
    let pre_goal =
      Goal.replace goal ~post_cond:pre_assert ~left_stmts:[] ~right_stmts:[]
    in
    (* Assume the mutual postcondition *)
    let tmp_retvars =
      List.map (List.append left_proc.Procedure.rets right_proc.Procedure.rets)
        ~f:(fun vb ->
          let name = FreshNameGenerator.get_fresh_var ~name:"$reltmpret" () in
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
    let post_goal = Goal.replace goal ~pre_conds ~left_stmts ~right_stmts in
    (pre_goal, post_goal)
  in
  match lookup_summary env left_proc right_proc with
  | None ->
      let env, pre_pred, post_pred = create_summary env left_proc right_proc in
      let target_bindings =
        List.concat
          [ left_proc.params
          ; left_proc.rets
          ; left_proc.locals
          ; right_proc.params
          ; right_proc.rets
          ; right_proc.locals ]
      in
      let body_havoc = Precondition.mk_havoc target_bindings in
      let param_bindings = List.append left_proc.params right_proc.params in
      let ret_bindings = List.append left_proc.rets right_proc.rets in
      let body_pre =
        Precondition.mk_predicate pre_pred.name
          (List.map param_bindings ~f:Expr.mk_var)
      in
      (* It is VERY important that in the postcondition, we should use a COPY of the parameters instead of the parameters themselves directly. *)
      (* If we don't do that, the proof system becomes unsound when the parameter is modified in the function body *)
      let param_copies =
        List.map param_bindings ~f:(fun vb ->
            let name =
              FreshNameGenerator.get_fresh_var ~name:"$reltmpparam" ()
            in
            VarBinding.{vb with name} )
      in
      let param_copy_pres =
        List.map (List.zip_exn param_bindings param_copies) ~f:
          (fun (param, copy) ->
            Precondition.mk_assign (Lvalue.mk_var copy) (Expr.mk_var param) )
      in
      let body_post =
        let post_args =
          List.map (List.append ret_bindings param_copies) ~f:Expr.mk_var
        in
        Postcondition.mk_predicate post_pred.name post_args
      in
      let body_goal =
        Goal.mk
          (List.rev_append param_copy_pres [body_pre; body_havoc])
          body_post left_proc.name left_proc.stmts right_proc.name
          right_proc.stmts
      in
      let pre_goal, post_goal = do_apply_impl pre_pred.name post_pred.name in
      Some ([pre_goal; body_goal; post_goal], env)
  | Some (pre_pred, post_pred) ->
      let pre_goal, post_goal = do_apply_impl pre_pred.name post_pred.name in
      Some ([pre_goal; post_goal], env)

let apply ast env _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | ( Stmt.({ bare_stmt=
                Call {rets= left_rets; name= left_name; args= left_args}; _ })
      :: left_stmts
    , Stmt.({ bare_stmt=
                Call {rets= right_rets; name= right_name; args= right_args}; _
            })
      :: right_stmts ) ->
      let left_proc = lookup_proc_exn ast left_name in
      let right_proc = lookup_proc_exn ast right_name in
      do_apply left_rets left_proc left_args left_stmts right_rets right_proc
        right_args right_stmts env goal
  | _ -> None

let synccall = SemiLocalRule.mk_rule ~is_applicable ~name:"synccall" apply
