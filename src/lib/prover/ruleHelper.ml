open Core
open Ast.Ecoeus

let insert_locals ast proc_name bindings =
  let proc = lookup_proc_exn ast proc_name in
  let locals = List.append bindings proc.locals in
  let proc' = Procedure.{proc with locals} in
  update_proc ast proc'

let mk_symmetric_local_rule ~apply_left side ast depth goal =
  match side with
  | Side.Left -> apply_left ast depth goal
  | Side.Right ->
      (* Swap first *)
      let goal_swapped = Goal.swap_stmts goal in
      let open Option in
      apply_left ast depth goal_swapped
      >>= fun goal_swapped' ->
      (* Then swap it back *)
      let goal = Goal.swap_stmts goal_swapped' in
      Some goal

let mk_symmetric_semilocal_rule ~apply_left side ast env depth goal =
  match side with
  | Side.Left -> apply_left ast env depth goal
  | Side.Right ->
      (* Swap first *)
      let goal = Goal.swap_stmts goal in
      let open Option in
      apply_left ast env depth goal
      >>= fun (goals, env) ->
      (* Then swap it back *)
      let goals = List.map goals ~f:Goal.swap_stmts in
      Some (goals, env)

let mk_symmetric_quasilocal_rule ~apply_left side ast env depth goal =
  match side with
  | Side.Left -> apply_left ast env depth goal
  | Side.Right ->
      (* Swap first *)
      let goal = Goal.swap_stmts goal in
      let open Option in
      apply_left ast env depth goal
      >>= fun (goals, env, ast) ->
      (* Then swap it back *)
      let goals = List.map goals ~f:Goal.swap_stmts in
      Some (goals, env, ast)

(* Why does Core.Hash_set contain an intersection API but not a union API? *)
let merge_varsets s0 s1 =
  let do_merge sfrom sto =
    let res = Hash_set.copy sto in
    Hash_set.iter sfrom ~f:(fun elem -> Hash_set.add res elem) ;
    res
  in
  if Hash_set.length s0 < Hash_set.length s1 then do_merge s1 s0
  else do_merge s0 s1

let create_rename_map =
  List.fold ~init:Identifier.Map.empty ~f:(fun acc VarBinding.({name= v; _}) ->
      let name = Identifier.string_of v in
      let v' = FreshNameGenerator.get_fresh_var ~name () in
      Identifier.Map.set acc ~key:v ~data:v' )

(* Given two stmt sequences, create a new proof goal which says that the two sequences are semantically equivalent, i.e. if they start with the same program state they will end up with the same state as well *)
let create_equivalent_stmts_goal ast stmts0 stmts1 =
  let write_varset0 = Stmts.write_var_set_of stmts0 in
  let write_varset1 = Stmts.write_var_set_of stmts1 in
  match Hash_set.equal write_varset0 write_varset1 with
  | false ->
      (* It's impossible for stmts0 and stmts1 to be equivalent as they may write to different vars *)
      None
  | true ->
      let read_varset = Stmts.read_var_set_of (List.append stmts0 stmts1) in
      let all_vars =
        Hash_set.to_list (merge_varsets write_varset0 read_varset)
      in
      let rename_map0 = create_rename_map all_vars in
      let rename_map1 = create_rename_map all_vars in
      let locals0, locals1 =
        List.fold all_vars ~init:([], []) ~f:(fun (locals0, locals1) vb ->
            let name0 = Identifier.Map.find_exn rename_map0 vb.name in
            let name1 = Identifier.Map.find_exn rename_map1 vb.name in
            let locals0 = {vb with name= name0} :: locals0 in
            let locals1 = {vb with name= name1} :: locals1 in
            (locals0, locals1) )
      in
      (* The signatures can be arbitrary since nobody would really invoke these dummy functions *)
      let proc_name0 = FreshNameGenerator.get_fresh_proc () in
      let proc_name1 = FreshNameGenerator.get_fresh_proc () in
      let stmts0 =
        Stmts.map_var stmts0 ~f:(Identifier.Map.find_exn rename_map0)
      in
      let stmts1 =
        Stmts.map_var stmts1 ~f:(Identifier.Map.find_exn rename_map1)
      in
      let left_proc =
        let open Procedure in
        { name= proc_name0
        ; params= []
        ; rets= []
        ; locals= locals0
        ; stmts= Stmts.with_parent stmts0 proc_name0 }
      in
      let right_proc =
        let open Procedure in
        { name= proc_name1
        ; params= []
        ; rets= []
        ; locals= locals1
        ; stmts= Stmts.with_parent stmts1 proc_name1 }
      in
      let freevars =
        let bindings = List.append locals0 locals1 in
        Verifier.Precondition.mk_havoc bindings
      in
      let eq_vars (vb: VarBinding.t) =
        let v0 = Identifier.Map.find_exn rename_map0 vb.name in
        let v1 = Identifier.Map.find_exn rename_map1 vb.name in
        Expr.mk_binary BinaryOperator.Eq
          (Expr.mk_var {vb with name= v0})
          (Expr.mk_var {vb with name= v1})
      in
      let assumes = List.map all_vars ~f:eq_vars in
      let pre_conds = [Verifier.Precondition.mk_assume assumes; freevars] in
      let asserts = List.map (Hash_set.to_list write_varset0) ~f:eq_vars in
      let post_cond =
        Verifier.Postcondition.mk_assert ~is_final:true asserts
      in
      let goal =
        Goal.mk pre_conds post_cond proc_name0 stmts0 proc_name1 stmts1
      in
      let procs = right_proc :: left_proc :: ast.procs in
      let ast = {ast with procs} in
      Some (goal, ast)
