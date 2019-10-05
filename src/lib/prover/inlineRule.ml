open Core
open Verifier
open Ast.Ecoeus

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with s :: _ when Stmt.is_call s -> true | _ -> false

let inline_proc parent rets (target_proc: Procedure.t) args =
  let create_subst_map bindings vals =
    let names = List.map bindings ~f:(fun VarBinding.({name; _}) -> name) in
    let alist = List.zip_exn names vals in
    List.fold alist ~init:Identifier.Map.empty ~f:(fun acc (key, data) ->
        Identifier.Map.set acc ~key ~data )
  in
  let body = Stmts.with_parent target_proc.stmts parent in
  let param_vars =
    List.map target_proc.params ~f:(fun vb ->
        let name = FreshNameGenerator.get_fresh_var ~name:"inline_param" () in
        VarBinding.{vb with name} )
  in
  let ret_vars =
    List.map target_proc.rets ~f:(fun vb ->
        let name = FreshNameGenerator.get_fresh_var ~name:"inline_ret" () in
        VarBinding.{vb with name} )
  in
  let free_vars = List.append param_vars ret_vars in
  let havoc_pre = Precondition.mk_havoc free_vars in
  let subst_map =
    create_subst_map
      (List.append target_proc.params target_proc.rets)
      (List.map free_vars ~f:(fun VarBinding.({name; _}) -> name))
  in
  let body =
    Stmts.map_var body ~f:(fun id ->
        match Identifier.Map.find subst_map id with
        | Some id' -> id'
        | None -> id )
  in
  let param_assign_stmts =
    let param_pairs = List.zip_exn args param_vars in
    List.map param_pairs ~f:(fun (arg, vb) ->
        let lhs = Lvalue.mk_var vb in
        Stmt.mk_assign parent lhs arg )
  in
  let ret_assign_stmts =
    let ret_pairs = List.zip_exn rets ret_vars in
    List.map ret_pairs ~f:(fun (lhs, vb) ->
        let rhs = Expr.mk_var vb in
        Stmt.mk_assign parent lhs rhs )
  in
  (havoc_pre, List.concat [param_assign_stmts; body; ret_assign_stmts])

let apply_left ast _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({bare_stmt= Call {rets; name; args}; _}) :: rest ->
      let target_proc = lookup_proc_exn ast name in
      let havoc_pre, inlined_body =
        inline_proc goal.left_proc rets target_proc args
      in
      let pre_conds = havoc_pre :: goal.pre_conds in
      let left_stmts = List.append inlined_body rest in
      let goal' = Goal.replace goal ~pre_conds ~left_stmts in
      Some goal'
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_local_rule ~apply_left side in
  LocalRule.mk_rule ~name ~is_applicable:(is_applicable side) apply

let inline_l = create "inline_l" Side.Left

let inline_r = create "inline_r" Side.Right
