open Core
open Ast.Ecoeus

let z3_query_sexp =
  Sexp.List
    [ Sexp.Atom "query"
    ; Sexp.Atom (Identifier.string_of PredSignature.z3_query_id) ]

let z3_binding_sexp_of (name, ty) =
  let name_sexp = Identifier.sexp_of_t name in
  let ty_sexp = Type.sexp_of_t ty in
  Sexp.List [Sexp.Atom "declare-var"; name_sexp; ty_sexp]

let sexp_of_predicate name args =
  Sexp.List (Identifier.sexp_of_t name :: List.map ~f:Expr.sexp_of_t args)

let conjunct_sexps = function
  | [] -> Sexp.Atom "true"
  | [s] -> s
  | es -> Sexp.List (Sexp.Atom "and" :: es)

module PathCondition = struct
  type t =
    | Expr of Expr.t
    | Predicate of {name: Identifier.t; args: Expr.t list}

  let to_sexp = function
    | Expr e -> Expr.sexp_of_t e
    | Predicate {name; args} ->
      match args with
      | [] -> Identifier.sexp_of_t name
      | _ -> sexp_of_predicate name args
end

module Env = struct
  type t =
    {path_conds: PathCondition.t list; let_defs: (Identifier.t * Expr.t) list}

  let create () = {path_conds= []; let_defs= []}

  let add_path_cond env cond =
    let path_conds = cond :: env.path_conds in
    {env with path_conds}

  let add_let_def env v e =
    let let_defs = (v, e) :: env.let_defs in
    {env with let_defs}

  let to_sexps {let_defs; path_conds; _} =
    match path_conds with
    | [] -> failwith "path cond is empty"
    | post_cond :: pre_conds ->
        let pre_sexp =
          conjunct_sexps (List.rev_map ~f:PathCondition.to_sexp pre_conds)
        in
        let post_sexp = PathCondition.to_sexp post_cond in
        let init = Sexp.List [Sexp.Atom "=>"; pre_sexp; post_sexp] in
        List.fold let_defs ~init ~f:(fun acc (v, e) ->
            let bind_sexp =
              Sexp.List [Identifier.sexp_of_t v; Expr.sexp_of_t e]
            in
            Sexp.List [Sexp.Atom "let"; Sexp.List [bind_sexp]; acc] )
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
      let lhs_var = lhs.base.name in
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

let encode_vc VerifCondition.({pre_conds; post_cond; _}) =
  let init = Env.create () in
  let env = List.fold (List.rev pre_conds) ~init ~f:process_pre in
  let env = process_post env post_cond in
  let imply_sexp = Env.to_sexps env in
  Sexp.List [Sexp.Atom "rule"; imply_sexp]

let encode_sexp_directly VerifState.({pred_env; verif_conds; _}) =
  let decls = PredSignature.z3_query_rel :: PredEnv.signatures pred_env in
  let vc_sexps, bindings =
    List.fold verif_conds ~init:([], Identifier.Map.empty) ~f:
      (fun (acc, vmap) vc ->
        let vc_sexp = encode_vc vc in
        let acc' = vc_sexp :: acc in
        let vc_bindings = VerifCondition.free_vars_of vc in
        let vmap' =
          List.fold vc_bindings ~init:vmap ~f:
            (fun acc VarBinding.({name; ty}) ->
              Identifier.Map.update acc name ~f:(function
                | None -> ty
                | Some ty' ->
                    if not (Type.equal ty ty') then
                      let msg =
                        "[INTERNAL] Inconsistent binding during z3 sexp \
                         construction"
                      in
                      failwith msg
                    else ty ) )
        in
        (acc', vmap') )
  in
  let decl_sexps = List.map decls ~f:PredSignature.z3rel_of_t in
  let binding_sexps =
    List.map (Identifier.Map.to_alist bindings) ~f:z3_binding_sexp_of
  in
  (List.append decl_sexps binding_sexps, vc_sexps, z3_query_sexp)

let preprocess vstate =
  let vstate = LvalueSimplifier.run vstate in
  let vstate = FunCallEliminator.run vstate in
  let vstate = DeadAssignEliminator.run vstate in
  let vstate = HavocEliminator.run vstate in
  let vstate = VarRenamer.run vstate in
  vstate

let encode_sexp vstate = encode_sexp_directly (preprocess vstate)
