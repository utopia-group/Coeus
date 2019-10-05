open Core
open Ast.Ecoeus
open Verifier

let encode_pred
    PredEnv.Info.({signature= PredSignature.({name; params}); source}) =
  let hints = HoudiniTemplate.guess source in
  HornAst.Predicate.{name; params; hints}

let encode_vc idx VerifCondition.({pre_conds; post_cond; _} as vc) =
  let open HornAst.Predicate in
  let open HornAst.Rule in
  let encode_pre pre_cond =
    let open Precondition in
    match pre_cond.bare with
    | Havoc _ ->
        let msg =
          "HornEncoder cannot handle havocs. Please run HavocEliminator first"
        in
        failwith msg
    | Assign _ ->
        let msg =
          "HornEncoder cannot handle assignments. Please run AssignInliner \
           first"
        in
        failwith msg
    | Assume es -> List.map es ~f:(fun e -> Body.Expr e)
    | Predicate {name; args} -> [Body.Predicate Apply.{name; args}]
  in
  let encode_post pre_conds post_cond =
    let open Postcondition in
    match post_cond.bare with
    | Assert {exprs; _} ->
        let cexpr = Expr.conjunct_exprs exprs in
        let nexpr = Expr.logical_negate_of cexpr in
        let assume = Precondition.mk_assume [nexpr] in
        (assume :: pre_conds, Head.False)
    | Predicate {name; args} ->
        let head = Head.Predicate Apply.{name; args} in
        (pre_conds, head)
  in
  let pre_conds, head = encode_post pre_conds post_cond in
  let bodies = List.concat_map (List.rev pre_conds) ~f:encode_pre in
  let bindings = VerifCondition.free_vars_of vc in
  let name = Identifier.of_string (Fmt.strf "RULE%d" idx) in
  {name; bindings; head; bodies}

let encode_impl VerifState.({pred_env; verif_conds; _}) =
  let preds = PredEnv.map pred_env ~f:encode_pred in
  let rules = List.mapi verif_conds ~f:encode_vc in
  HornAst.create preds rules

let encode vstate =
  let vstate = LvalueSimplifier.run vstate in
  let vstate = DeadAssignEliminator.run vstate in
  let vstate = AssignInliner.run vstate in
  let vstate = HavocEliminator.run vstate in
  encode_impl vstate
