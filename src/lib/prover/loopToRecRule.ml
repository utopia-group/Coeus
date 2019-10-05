open Core
open Verifier
open Ast.Ecoeus

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with s :: _ when Stmt.is_loop s -> true | _ -> false

let create_proc ast cond body param_bindings ret_vars =
  let proc_name = FreshNameGenerator.get_fresh_proc ~name:"looptorec" () in
  let ret_bindings =
    List.map ret_vars ~f:(fun vb ->
        let name = FreshNameGenerator.get_fresh_var ~name:"tmpret" () in
        VarBinding.{vb with name} )
  in
  let body = Stmts.with_parent body proc_name in
  let term_stmts =
    List.map (List.zip_exn ret_bindings ret_vars) ~f:
      (fun (formal_ret, actual_ret) ->
        let lhs = Lvalue.mk_var formal_ret in
        let rhs = Expr.mk_var actual_ret in
        Stmt.mk_assign proc_name lhs rhs )
  in
  let loop_stmts =
    let lvals = List.map ret_bindings ~f:Lvalue.mk_var in
    let args = List.map param_bindings ~f:Expr.mk_var in
    let call_stmt = Stmt.mk_call proc_name lvals proc_name args in
    List.append body [call_stmt]
  in
  (* Terminators usually comes first *)
  let if_stmt =
    Stmt.mk_if proc_name (Expr.logical_negate_of cond) term_stmts loop_stmts
  in
  let new_proc =
    let open Procedure in
    { name= proc_name
    ; params= param_bindings
    ; rets= ret_bindings
    ; locals= []
    ; stmts= [if_stmt] }
  in
  let procs = new_proc :: ast.procs in
  let ast = {ast with procs} in
  (ast, new_proc)

let apply_left_while cond body left_stmts ast (goal: Goal.t) =
  let while_stmt = Stmt.mk_while goal.left_proc cond body in
  let rw_vars =
    let rw_varset =
      Stmt.read_write_var_set_of ~exclude_counter:false while_stmt
    in
    Hash_set.to_list rw_varset
  in
  let write_vars =
    let write_varset =
      Stmt.write_var_set_of ~exclude_counter:false while_stmt
    in
    Hash_set.to_list write_varset
  in
  let ast, rec_proc = create_proc ast cond body rw_vars write_vars in
  let call_stmt =
    Stmt.mk_call goal.left_proc
      (List.map write_vars ~f:Lvalue.mk_var)
      rec_proc.name
      (List.map rw_vars ~f:Expr.mk_var)
  in
  let left_stmts = call_stmt :: left_stmts in
  let goal = Goal.replace goal ~left_stmts in
  (ast, goal)

let apply_left ast env _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({bare_stmt= While {cond; body}; _}) :: left_stmts ->
      let ast', goal' = apply_left_while cond body left_stmts ast goal in
      Some ([goal'], env, ast')
  | Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _} as
          s)
    :: left_stmts ->
      let parent = Stmt.parent_of s in
      let lhs, rhs, cond, body =
        Stmt.while_of_for parent counter lower upper step direction body
      in
      let pre = Precondition.mk_assign lhs rhs in
      let pre_conds = pre :: goal.pre_conds in
      let goal = Goal.replace goal ~pre_conds in
      let ast', goal' = apply_left_while cond body left_stmts ast goal in
      Some ([goal'], env, ast')
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_quasilocal_rule ~apply_left side in
  QuasiLocalRule.mk_rule ~name ~is_applicable:(is_applicable side) apply

let looptorec_l = create "looptorec_l" Side.Left

let looptorec_r = create "looptorec_r" Side.Right
