open Core
open Ast.Ecoeus

let horn_prelude_sexp = Sexp.List [Sexp.Atom "set-logic"; Sexp.Atom "HORN"]

let horn_checksat_sexp = Sexp.List [Sexp.Atom "check-sat"]

let sexp_of_predicate name args =
  Sexp.List (Identifier.sexp_of_t name :: List.map ~f:Expr.sexp_of_t args)

let encode_vc_sexp VerifCondition.({pre_conds; post_cond; _}) =
  let rec encode_vc_impl post_cond = function
    | [] -> (
        let open Postcondition in
        match post_cond.bare with
        | Assert {exprs; _} -> Expr.sexp_of_t (Expr.conjunct_exprs exprs)
        | Predicate {name; args} -> sexp_of_predicate name args )
    | pre :: pres ->
        let rest_sexp = encode_vc_impl post_cond pres in
        let open Precondition in
        match pre.bare with
        | Havoc vs ->
            let bind_sexp = Sexp.List (List.map ~f:VarBinding.sexp_of_t vs) in
            Sexp.List [Sexp.Atom "forall"; bind_sexp; rest_sexp]
        | Assume es ->
            let e = Expr.conjunct_exprs es in
            Sexp.List [Sexp.Atom "=>"; Expr.sexp_of_t e; rest_sexp]
        | Predicate {name; args} ->
            let pred_sexp = sexp_of_predicate name args in
            Sexp.List [Sexp.Atom "=>"; pred_sexp; rest_sexp]
        | Assign (Lvalue.({base; indices; _}), rhs) ->
            let bind_sexp =
              match indices with
              | [] ->
                  Sexp.List [Identifier.sexp_of_t base.name; Expr.sexp_of_t rhs]
              | _ ->
                  let msg =
                    "EldaricaEncoder cannot handle array-based lvalues. \
                     Please run LvalueSimplifier first"
                  in
                  failwith msg
            in
            Sexp.List [Sexp.Atom "let"; Sexp.List [bind_sexp]; rest_sexp]
  in
  Sexp.List [Sexp.Atom "assert"; encode_vc_impl post_cond (List.rev pre_conds)]

let encode_sexp_impl VerifState.({pred_env; verif_conds; _}) =
  let decls = PredEnv.signatures pred_env in
  let decl_sexps = List.map decls ~f:PredSignature.smtlib_of_t in
  let vc_sexps = List.map verif_conds ~f:encode_vc_sexp in
  horn_prelude_sexp :: List.concat [decl_sexps; vc_sexps; [horn_checksat_sexp]]

let encode_sexp vstate =
  let vstate = LvalueSimplifier.run vstate in
  let vstate = FunCallEliminator.run vstate in
  let vstate = DeadAssignEliminator.run vstate in
  let vstate = AssignInliner.run vstate in
  encode_sexp_impl vstate
