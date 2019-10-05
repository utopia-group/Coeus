open Core
open Ast.Ecoeus

module FreshNameGen = struct
  type t = {next_var_id: int ref; next_pred_id: int ref}

  let create () = {next_var_id= ref 0; next_pred_id= ref 0}

  let next prefix idref =
    let id = !idref in
    incr idref ;
    let str = Fmt.strf "%s%d" prefix id in
    Identifier.of_string str

  let next_var {next_var_id; _} = next "v" next_var_id

  let next_pred {next_pred_id; _} = next "P" next_pred_id
end

module RenameEnv = struct
  type t =
    { fresh_gen: FreshNameGen.t
    ; var_table: VarBinding.t Identifier.Table.t
    ; pred_table: Identifier.t Identifier.Table.t }

  let create () =
    { fresh_gen= FreshNameGen.create ()
    ; var_table= Identifier.Table.create ()
    ; pred_table= Identifier.Table.create () }

  let add_predicate {fresh_gen; pred_table; _} (pred : PredSignature.t) =
    let new_name = FreshNameGen.next_pred fresh_gen in
    Identifier.Table.add_exn pred_table ~key:pred.name ~data:new_name

  let add_binding {fresh_gen; var_table; _} (vb : VarBinding.t) =
    let new_name = FreshNameGen.next_var fresh_gen in
    let new_vb = VarBinding.{name= new_name; ty= vb.ty} in
    Identifier.Table.set var_table ~key:vb.name ~data:new_vb

  let var_map_f {var_table; _} v =
    match Identifier.Table.find var_table v with
    | None -> v
    | Some vb -> vb.name

  let pred_map_f {pred_table; _} pred =
    match Identifier.Table.find pred_table pred with
    | Some name -> name
    | None -> pred

  let pred_subst {pred_table; _} = Identifier.Table.find pred_table

  let pred_subst_exn env name =
    match pred_subst env name with
    | Some n -> n
    | None ->
        let msg =
          Fmt.strf
            "[INTERNAL] RenameEnv.pred_subst failed to find predicate %a"
            Identifier.pp name
        in
        failwith msg

  let reset_var {var_table; _} = Identifier.Table.clear var_table
end

let subst_precond env pre_cond =
  let open Precondition in
  match pre_cond.bare with
  | Havoc _ ->
      let msg =
        Fmt.strf
          "VarRenamer cannot handle havocs. Please run HavocEliminator first"
      in
      failwith msg
  | Assume es ->
      let es' = List.map es ~f:(Expr.map_var ~f:(RenameEnv.var_map_f env)) in
      mk_assume es'
  | Predicate {name; args} ->
      let name' = RenameEnv.pred_subst_exn env name in
      let args' =
        List.map args ~f:(Expr.map_var ~f:(RenameEnv.var_map_f env))
      in
      mk_predicate name' args'
  | Assign (lhs, rhs) -> (
      let base = lhs.base in
      match lhs.indices with
      | [] ->
          RenameEnv.add_binding env base ;
          let lhs' = Lvalue.map_var lhs ~f:(RenameEnv.var_map_f env) in
          let rhs' = Expr.map_var ~f:(RenameEnv.var_map_f env) rhs in
          mk_assign lhs' rhs'
      | _ ->
          let msg =
            Fmt.strf
              "VarRenamer cannot handle array-based lvalues. Please run \
               LvalueSimplifier first"
          in
          failwith msg )

let subst_postcond env post_cond =
  let open Postcondition in
  match post_cond.bare with
  | Assert {exprs; is_final} ->
      let exprs' =
        List.map exprs ~f:(Expr.map_var ~f:(RenameEnv.var_map_f env))
      in
      mk_assert exprs' ~is_final
  | Predicate {name; args} ->
      let name' = RenameEnv.pred_subst_exn env name in
      let args' =
        List.map args ~f:(Expr.map_var ~f:(RenameEnv.var_map_f env))
      in
      mk_predicate name' args'

let run_vc env VerifCondition.({pre_conds; post_cond; blame_step; _} as vc) =
  RenameEnv.reset_var env ;
  let free_vars = VerifCondition.free_vars_of vc in
  List.iter free_vars ~f:(RenameEnv.add_binding env) ;
  let pre_conds = List.rev_map (List.rev pre_conds) ~f:(subst_precond env) in
  let post_cond = subst_postcond env post_cond in
  VerifCondition.mk ?blame_step pre_conds post_cond

let run VerifState.({verif_conds; pred_env; _}) =
  let env = RenameEnv.create () in
  let pred_sigs = PredEnv.signatures pred_env in
  List.iter pred_sigs ~f:(RenameEnv.add_predicate env) ;
  let pred_env' = PredEnv.rename pred_env ~f:(RenameEnv.pred_map_f env) in
  let verif_conds' = List.map verif_conds ~f:(run_vc env) in
  let vs = VerifState.mk pred_env' verif_conds' in
  vs
