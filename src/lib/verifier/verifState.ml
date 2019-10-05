open Core
open Ast.Ecoeus

type t =
  {pred_env: PredEnv.t; verif_conds: VerifCondition.t list; ast_size: int}
[@@deriving sexp, compare]

let empty = {pred_env= PredEnv.create (); verif_conds= []; ast_size= 1}

let mk pred_env verif_conds =
  let ast_size =
    1 + PredEnv.length pred_env
    + List.fold verif_conds ~init:0 ~f:(fun acc vc ->
          acc + VerifCondition.ast_size_of vc )
  in
  {pred_env; verif_conds; ast_size}

let with_env {pred_env; verif_conds; ast_size} new_env =
  let old_len = PredEnv.length pred_env in
  let new_len = PredEnv.length new_env in
  let ast_size = ast_size - old_len + new_len in
  {pred_env= new_env; verif_conds; ast_size}

let ast_size_of {ast_size; _} = ast_size

let pp fmt {pred_env; verif_conds; _} =
  let decls = PredEnv.signatures pred_env in
  Fmt.pf fmt "VerifierState(DECLS = { %a }, VCs = [ %a ])"
    (Fmt.list ~sep:Fmt.comma PredSignature.pp)
    decls
    (Fmt.list ~sep:Fmt.comma VerifCondition.pp)
    verif_conds

let add_vc state vc =
  let verif_conds = vc :: state.verif_conds in
  let ast_size = state.ast_size + VerifCondition.ast_size_of vc in
  {state with ast_size; verif_conds}

let add_decl ?source state decl =
  let pred_env = PredEnv.extend ?source state.pred_env decl in
  let ast_size = state.ast_size + 1 in
  {state with ast_size; pred_env}

(* Validation logic *)

exception ValidationError of string

module ValidationEnv = struct
  type t = {pred_env: PredSignature.t Identifier.Map.t}

  let init decls =
    let pred_env =
      List.fold decls ~init:Identifier.Map.empty ~f:(fun acc pred ->
          let key = pred.PredSignature.name in
          Identifier.Map.update acc key ~f:(function
            | Some _ ->
                let msg =
                  Fmt.strf "Duplicate predicate definition %a" Identifier.pp
                    key
                in
                raise (ValidationError msg)
            | None -> pred ) )
    in
    {pred_env}
end

let validate_expr env ty e =
  let ty' = Expr.type_of e in
  if Type.equal ty ty' then env
  else
    let msg =
      Fmt.strf
        "Type inconsistency in expression \"%a\": expected %a but the actual \
         type is %a"
        Expr.pp e Type.pp ty Type.pp ty'
    in
    raise (ValidationError msg)

let validate_assign env lhs rhs =
  let lhs_ty = Lvalue.type_of lhs in
  validate_expr env lhs_ty rhs

let validate_args env (pred_sig: PredSignature.t) args =
  match List.zip (PredSignature.param_types_of pred_sig) args with
  | None ->
      let msg =
        Fmt.strf
          "Inconsistent number of arguments for predicate \"%a\": expected %d \
           but passed %d"
          Identifier.pp pred_sig.name
          (List.length pred_sig.params)
          (List.length args)
      in
      raise (ValidationError msg)
  | Some pairs ->
      List.fold pairs ~init:env ~f:(fun acc (ty, e) -> validate_expr acc ty e)

let validate_pred env name args =
  match Identifier.Map.find env.ValidationEnv.pred_env name with
  | None ->
      let msg = Fmt.strf "Undefined predicate %a" Identifier.pp name in
      raise (ValidationError msg)
  | Some pred -> validate_args env pred args

let validate_pre env pre =
  let open Precondition in
  match pre.bare with
  | Havoc _ -> env
  | Assume es ->
      List.fold es ~init:env ~f:(fun acc e -> validate_expr acc Type.BoolType e)
  | Assign (lhs, rhs) -> validate_assign env lhs rhs
  | Predicate {name; args} -> validate_pred env name args

let validate_post env post =
  let open Postcondition in
  match post.bare with
  | Assert {exprs; _} ->
      List.fold exprs ~init:env ~f:(fun acc e ->
          validate_expr acc Type.BoolType e )
  | Predicate {name; args} -> validate_pred env name args

let validate_vc env VerifCondition.({pre_conds; post_cond; _}) =
  let env = List.fold pre_conds ~init:env ~f:validate_pre in
  validate_post env post_cond

let validate {pred_env; verif_conds; _} =
  try
    let decls = PredEnv.signatures pred_env in
    let env = ValidationEnv.init decls in
    let _ = List.fold verif_conds ~init:env ~f:validate_vc in
    Result.Ok ()
  with ValidationError msg -> Result.Error msg
