open Core
open Ast.Ecoeus
open Verifier

let encode_pred env name param_tys =
  let fundecl = FunDecl.{name; param_tys; ret_ty= Type.BoolType} in
  Z3Encoder.encode_fundecl env fundecl

let encode_decl env PredSignature.({name; params}) =
  let param_tys = List.map params ~f:(fun VarBinding.({ty; _}) -> ty) in
  encode_pred env name param_tys

(* TODO: Most of the code are copied from SpacerEncoder. Try to merge the common functionalities *)

module PathCondition = struct
  type t =
    | Expr of Expr.t
    | Predicate of {name: Identifier.t; args: Expr.t list}

  let pp fmt = function
    | Expr e -> Expr.pp fmt e
    | Predicate {name; args} ->
        let dummy_decl =
          FunDecl.{name; param_tys= []; ret_ty= Type.BoolType}
        in
        Expr.pp fmt (Expr.mk_funcall dummy_decl args)

  let to_z3 env = function
    | Expr e -> Z3Encoder.encode_expr env e
    | Predicate {name; args} ->
        let param_tys = List.map args ~f:Expr.type_of in
        let zdecl = encode_pred env name param_tys in
        let zargs = List.map args ~f:(Z3Encoder.encode_expr env) in
        Z3.FuncDecl.apply zdecl zargs
end

module Env = struct
  type t =
    {path_conds: PathCondition.t list; let_defs: (VarBinding.t * Expr.t) list}

  let create () = {path_conds= []; let_defs= []}

  let add_path_cond env cond =
    let path_conds = cond :: env.path_conds in
    {env with path_conds}

  let add_let_def env v e =
    let let_defs = (v, e) :: env.let_defs in
    {env with let_defs}

  let to_z3 z3env {let_defs; path_conds; _} =
    match path_conds with
    | [] -> failwith "path cond is empty"
    | post_cond :: pre_conds ->
        let zpre =
          Z3.Boolean.mk_and z3env.Z3Env.ctx
            (List.rev_map
               ~f:(fun c ->
                 Logs.debug (fun m -> m "c = %a" PathCondition.pp c) ;
                 PathCondition.to_z3 z3env c )
               pre_conds)
        in
        let zpost = PathCondition.to_z3 z3env post_cond in
        let zinit = Z3.Boolean.mk_implies z3env.ctx zpre zpost in
        List.fold let_defs ~init:zinit ~f:(fun acc (v, e) ->
            let sub = Z3Encoder.encode_expr z3env (Expr.mk_var v) in
            let ze = Z3Encoder.encode_expr z3env e in
            Z3.Expr.substitute acc [sub] [ze] )
end

let process_pre env pre =
  let open Precondition in
  match pre.bare with
  | Havoc _ -> env
  | Assume es ->
      let cexpr = Expr.conjunct_exprs es in
      let cond = PathCondition.Expr cexpr in
      Env.add_path_cond env cond
  | Predicate {name; args} ->
      let cond = PathCondition.Predicate {name; args} in
      Env.add_path_cond env cond
  | Assign (lhs, rhs) ->
      let lhs_var = lhs.base in
      let rhs_expr =
        match lhs.indices with
        | [] -> rhs
        | _ ->
            let msg =
              Fmt.strf
                "Z3Encoder cannot handle array-based lvalues. Please run \
                 LvalueSimplifier first"
            in
            failwith msg
      in
      Env.add_let_def env lhs_var rhs_expr

let process_post env post =
  let open Postcondition in
  match post.bare with
  | Assert
      {exprs= [Expr.({bare_expr= LiteralExpr (Literal.BoolLit false); _})]; _} ->
      (* Replace false head with query relation, as some of the Z3 backend engine (e.g. spacer) does not support it *)
      let cond =
        PathCondition.Predicate {name= PredSignature.z3_query_id; args= []}
      in
      Env.add_path_cond env cond
  | Predicate {name; args} ->
      let cond = PathCondition.Predicate {name; args} in
      Env.add_path_cond env cond
  | Assert {exprs; is_final} ->
      let e = Expr.conjunct_exprs exprs in
      if is_final then
        let ne = Expr.logical_negate_of e in
        let pre_cond = PathCondition.Expr ne in
        let env = Env.add_path_cond env pre_cond in
        let post_cond =
          PathCondition.Predicate {name= PredSignature.z3_query_id; args= []}
        in
        Env.add_path_cond env post_cond
      else
        let cond = PathCondition.Expr e in
        Env.add_path_cond env cond

let encode_vc z3env VerifCondition.({pre_conds; post_cond; _} as vc) =
  let init = Env.create () in
  let env = List.fold (List.rev pre_conds) ~init ~f:process_pre in
  let env = process_post env post_cond in
  let free_vars = VerifCondition.free_vars_of vc in
  let zfree_vars =
    List.map free_vars ~f:(fun vb ->
        Expr.mk_var vb |> Z3Encoder.encode_expr z3env )
  in
  let zbody = Env.to_z3 z3env env in
  let zquant =
    Z3.Quantifier.mk_quantifier z3env.Z3Env.ctx true zfree_vars zbody None []
      [] None None
  in
  Z3.Quantifier.expr_of_quantifier zquant

let encode_impl env VerifState.({pred_env; verif_conds; _}) =
  let decls = PredSignature.z3_query_rel :: PredEnv.signatures pred_env in
  let zdecls = List.map decls ~f:(encode_decl env) in
  (* For some reason, orders of these horn clauses have very big impact on whether spacer can solve them or not *)
  (* Reverse the order of the rules to stay consistent with SpacerEncoder, as our benchmarks were mostly collected when SpacerEncoder was still used *)
  let rules = List.rev_map verif_conds ~f:(encode_vc env) in
  let zquery = Z3.FuncDecl.apply (List.hd_exn zdecls) [] in
  (zdecls, rules, zquery)

let encode env vstate =
  let vstate = LvalueSimplifier.run vstate in
  let vstate = FunCallEliminator.run vstate in
  let vstate = DeadAssignEliminator.run vstate in
  let vstate = HavocEliminator.run vstate in
  encode_impl env vstate
