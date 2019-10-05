open Core
open Ast.Ecoeus

module CallSetCounter = struct
  let counter = ref 0

  let get () =
    let res = !counter in
    incr counter ; res

  let reset () = counter := 0
end

module Env = struct
  module CallInfo = struct
    type t = {arg_vars: VarBinding.t list; ret_var: VarBinding.t}
  end

  type t =
    {pre_conds: Precondition.t list; call_map: CallInfo.t list Identifier.Map.t}

  let create () = {pre_conds= []; call_map= Identifier.Map.empty}

  let add_precond eenv pre_cond =
    let pre_conds = pre_cond :: eenv.pre_conds in
    {eenv with pre_conds}

  let set_callinfo eenv name call_info =
    let call_map =
      Identifier.Map.set eenv.call_map ~key:name ~data:call_info
    in
    {eenv with call_map}
end

let rec elim_funcall_expr eenv expr =
  let open Expr in
  match expr.bare_expr with
  | LiteralExpr _ | VarExpr _ -> (eenv, expr)
  | UnaryExpr (op, e) ->
      let eenv, e' = elim_funcall_expr eenv e in
      (eenv, mk_unary op e')
  | BinaryExpr (op, lhs, rhs) ->
      let eenv, lhs' = elim_funcall_expr eenv lhs in
      let eenv, rhs' = elim_funcall_expr eenv rhs in
      (eenv, mk_binary op lhs' rhs')
  | ArraySelectExpr {base; indices} ->
      let eenv, base = elim_funcall_expr eenv base in
      let eenv, indices = elim_funcall_exprs eenv indices in
      (eenv, mk_array_select base indices (Expr.type_of expr))
  | ArrayStoreExpr {base; indices; value} ->
      let eenv, base = elim_funcall_expr eenv base in
      let eenv, indices = elim_funcall_exprs eenv indices in
      let eenv, value = elim_funcall_expr eenv value in
      (eenv, mk_array_store base indices value)
  | CondExpr {cond; true_val; false_val} ->
      let eenv, cond = elim_funcall_expr eenv cond in
      let eenv, true_val = elim_funcall_expr eenv true_val in
      let eenv, false_val = elim_funcall_expr eenv false_val in
      (eenv, mk_cond cond true_val false_val)
  | QuantifiedExpr {quantifier; bindings; body} ->
      let eenv, body = elim_funcall_expr eenv body in
      (eenv, mk_quantified quantifier bindings body)
  | FunCallExpr {func= FunDecl.({name; ret_ty; _}); args} ->
      let eenv, args = elim_funcall_exprs eenv args in
      (* Generate a fresh var for the return value *)
      let callsite_id = CallSetCounter.get () in
      let ret_var =
        Identifier.of_string
          (Fmt.strf "%a_%d@RET" Identifier.pp name callsite_id)
      in
      let ret_binding = VarBinding.{name= ret_var; ty= ret_ty} in
      let eenv =
        let pre_cond = Precondition.mk_havoc [ret_binding] in
        Env.add_precond eenv pre_cond
      in
      let ret_var_expr = Expr.mk_var ret_binding in
      let process_args args call_infos =
        let arg_bindings =
          List.mapi args ~f:(fun idx e ->
              let vname =
                Fmt.strf "%a_%d@PARAM%d" Identifier.pp name callsite_id idx
              in
              let var = Identifier.of_string vname in
              VarBinding.{name= var; ty= Expr.type_of e} )
        in
        let call_info =
          Env.CallInfo.{arg_vars= arg_bindings; ret_var= ret_binding}
        in
        let eenv = Env.set_callinfo eenv name (call_info :: call_infos) in
        let eenv =
          List.fold (List.zip_exn arg_bindings args) ~init:eenv ~f:
            (fun eenv (v, e) ->
              let pre_cond = Precondition.mk_assign (Lvalue.mk_var v) e in
              Env.add_precond eenv pre_cond )
        in
        (arg_bindings, eenv)
      in
      let eenv =
        match Identifier.Map.find eenv.call_map name with
        | None ->
            (* If we haven't seen this function before, setup its call info *)
            let _, eenv = process_args args [] in
            eenv
        | Some call_infos ->
            (* If we have seen the function, add the constraint that same params lead to the same return *)
            let arg_vars, eenv = process_args args call_infos in
            List.fold call_infos ~init:eenv ~f:(fun eenv call_info ->
                let eq_param_conds =
                  List.map (List.zip_exn call_info.arg_vars arg_vars) ~f:
                    (fun (v0, v1) ->
                      Expr.mk_binary BinaryOperator.Eq (Expr.mk_var v0)
                        (Expr.mk_var v1) )
                in
                let eq_param_cond = Expr.conjunct_exprs eq_param_conds in
                let eq_ret_cond =
                  Expr.mk_binary BinaryOperator.Eq
                    (Expr.mk_var call_info.ret_var)
                    ret_var_expr
                in
                let eq_axiom =
                  Expr.mk_binary BinaryOperator.Imply eq_param_cond eq_ret_cond
                in
                let axiom_precond = Precondition.mk_assume [eq_axiom] in
                Env.add_precond eenv axiom_precond )
      in
      (eenv, ret_var_expr)

and elim_funcall_exprs eenv exprs =
  let eenv, rev_exprs' =
    List.fold exprs ~init:(eenv, []) ~f:(fun (eenv, eacc) e ->
        let eenv, e' = elim_funcall_expr eenv e in
        (eenv, e' :: eacc) )
  in
  (eenv, List.rev rev_exprs')

let elim_funcall_lval eenv (lval: Lvalue.t) =
  let eenv, indices' = elim_funcall_exprs eenv lval.indices in
  let lval' = Lvalue.mk_array lval.base indices' (Lvalue.type_of lval) in
  (eenv, lval')

let elim_funcall_precond eenv pre_cond =
  let open Precondition in
  match pre_cond.bare with
  | Havoc _ -> Env.add_precond eenv pre_cond
  | Assume es ->
      let eenv, es' = elim_funcall_exprs eenv es in
      Env.add_precond eenv (mk_assume es')
  | Assign (lhs, rhs) ->
      let eenv, lhs' = elim_funcall_lval eenv lhs in
      let eenv, rhs' = elim_funcall_expr eenv rhs in
      Env.add_precond eenv (mk_assign lhs' rhs')
  | Predicate {name; args} ->
      let eenv, args' = elim_funcall_exprs eenv args in
      Env.add_precond eenv (mk_predicate name args')

let elim_funcall_postcond eenv post_cond =
  let open Postcondition in
  match post_cond.bare with
  | Assert {exprs; is_final} ->
      let eenv, exprs = elim_funcall_exprs eenv exprs in
      (eenv, mk_assert ~is_final exprs)
  | Predicate {name; args} ->
      let eenv, args = elim_funcall_exprs eenv args in
      (eenv, mk_predicate name args)

(* Note that we intentionally uses a ElimFunCallEnv per VerifCondition. This decision would cause us to lose some semantics information during the transformation, but it also mitigates the problems of quadratic formula size blowup of this transformation. *)

let run_vc VerifCondition.({pre_conds; post_cond; blame_step; _}) =
  let eenv = Env.create () in
  CallSetCounter.reset () ;
  let eenv =
    List.fold (List.rev pre_conds) ~init:eenv ~f:elim_funcall_precond
  in
  let eenv, post_cond' = elim_funcall_postcond eenv post_cond in
  VerifCondition.mk ?blame_step eenv.Env.pre_conds post_cond'

let run VerifState.({verif_conds; pred_env; _}) =
  let verif_conds = List.map verif_conds ~f:run_vc in
  let vs = VerifState.mk pred_env verif_conds in
  vs
