open Core
open Ast.Ecoeus

module Env = struct
  type t = {counter: int; rename_map: VarBinding.t Identifier.Map.t}

  let create () = {counter= 0; rename_map= Identifier.Map.empty}

  let rec get_fresh env v =
    let vname = Fmt.strf "%a$%d" Identifier.pp v env.counter in
    let v' = Identifier.of_string vname in
    let counter = env.counter + 1 in
    let env = {env with counter} in
    if Identifier.Map.mem env.rename_map v' then get_fresh env v else (v', env)

  let havoc env (vb: VarBinding.t) =
    match Identifier.Map.find env.rename_map vb.name with
    | None ->
        let rename_map =
          Identifier.Map.set env.rename_map ~key:vb.name ~data:vb
        in
        (vb, {env with rename_map})
    | Some _ ->
        let v', env' = get_fresh env vb.name in
        let vb' = {vb with name= v'} in
        let rename_map =
          Identifier.Map.set env'.rename_map ~key:vb.name ~data:vb'
        in
        (vb', {env' with rename_map})

  let subst_lookup {rename_map; _} v =
    Option.map (Identifier.Map.find rename_map v) ~f:Expr.mk_var
end

let run_pre env pre_cond =
  let open Precondition in
  match pre_cond.bare with
  | Havoc vs ->
      let env =
        List.fold vs ~init:env ~f:(fun acc v ->
            let _, env = Env.havoc acc v in
            env )
      in
      (None, env)
  | Assign (lhs, rhs) -> (
    match lhs.indices with
    | [] ->
        let rhs = Expr.subst rhs ~f:(Env.subst_lookup env) in
        let base, env = Env.havoc env lhs.base in
        let lhs = Lvalue.mk_var base in
        let pre = mk_assign lhs rhs in
        (Some pre, env)
    | _ ->
        let msg =
          Fmt.strf
            "HavocEliminator cannot handle array-based lvalues. Please run \
             LvalueSimplifier first"
        in
        failwith msg )
  | Assume exprs ->
      let exprs = List.map exprs ~f:(Expr.subst ~f:(Env.subst_lookup env)) in
      let pre = mk_assume exprs in
      (Some pre, env)
  | Predicate {name; args} ->
      let args = List.map args ~f:(Expr.subst ~f:(Env.subst_lookup env)) in
      let pre = mk_predicate name args in
      (Some pre, env)

let run_post env post_cond =
  let open Postcondition in
  match post_cond.bare with
  | Assert {exprs; is_final} ->
      let exprs = List.map exprs ~f:(Expr.subst ~f:(Env.subst_lookup env)) in
      mk_assert ~is_final exprs
  | Predicate {name; args} ->
      let args = List.map args ~f:(Expr.subst ~f:(Env.subst_lookup env)) in
      mk_predicate name args

let run_vc VerifCondition.({pre_conds; post_cond; blame_step; _}) =
  let env = Env.create () in
  let pre_conds, env =
    List.fold (List.rev pre_conds) ~init:([], env) ~f:
      (fun (acc, env) pre_cond ->
        let opt_pre_cond, env' = run_pre env pre_cond in
        let acc' =
          Option.value_map ~default:acc opt_pre_cond ~f:(fun pre_cond ->
              pre_cond :: acc )
        in
        (acc', env') )
  in
  let post_cond = run_post env post_cond in
  VerifCondition.mk ?blame_step pre_conds post_cond

let run VerifState.({verif_conds; pred_env; _}) =
  let verif_conds = List.map verif_conds ~f:run_vc in
  let vs = VerifState.mk pred_env verif_conds in
  vs
