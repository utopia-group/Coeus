open Core
open Ast.Ecoeus

let apply_rule rule config state =
  let open Result in
  rule.Rule.apply config state
  >>= fun state ->
  let state = ProverState.resolve state in
  let state = ProverState.{state with depth= state.depth + 1} in
  Result.Ok state

(* It seems that using Rule.is_applicable is quite problematic here due to the inconsistencies it may lead to. *)
(* let applicable_candidate_rule_indices config state =
 *   let res =
 *     Array.foldi Rules.candidate_rules ~init:[] ~f:(fun id acc r ->
 *         if r.Rule.is_applicable config state then id :: acc else acc )
 *   in
 *   List.rev res
 * 
 * let applicable_candidate_rules config state =
 *   let indices = applicable_candidate_rule_indices config state in
 *   List.map indices ~f:(fun i -> Rules.candidate_rules.(i)) *)

let applicable_candidate_rule_indices_with_states config state =
  let res =
    Array.foldi Rules.candidate_rules ~init:[] ~f:(fun id acc r ->
        match apply_rule r config state with
        | Result.Ok s -> (id, s) :: acc
        | _ -> acc )
  in
  List.rev res

let applicable_candidate_rules_with_states config state =
  let id_states = applicable_candidate_rule_indices_with_states config state in
  List.map id_states ~f:(fun (i, s) -> (Rules.candidate_rules.(i), s))

let discharge solver_config state =
  if ProverState.is_fully_resolved state then
    Solver.Run.verify solver_config state.ProverState.verif_state
  else
    let msg =
      "Cannot discharge a prover state that has not been fully resolved"
    in
    Result.Error msg

let is_state_provable solver_config state =
  match discharge solver_config state with
  | Result.Ok status ->
      Logs.debug (fun m -> m "Solver return = %a" Solver.Status.pp status) ;
      status = Solver.Status.Verified
  | Result.Error msg ->
      (* Logs.debug (fun m -> m "Discharge failure: %s" msg) ; *)
      false

let prune_goal solver_config state =
  match state.ProverState.goals with
  | [] ->
      let msg = "Cannot find active goal" in
      Result.Error msg
  | Goal.({pre_conds; _}) :: rest ->
      let open Verifier in
      let open Result in
      let dummy_post_cond =
        Postcondition.mk_assert ~is_final:false
          [Expr.mk_literal (Literal.of_bool false)]
      in
      let dummy_verif_cond = VerifCondition.mk pre_conds dummy_post_cond in
      let dummy_verif_state =
        VerifState.mk state.verif_state.pred_env [dummy_verif_cond]
      in
      let dummy_proof_state =
        {state with goals= []; verif_state= dummy_verif_state}
      in
      if is_state_provable solver_config dummy_proof_state then
        Result.Ok {state with goals= rest}
      else
        let msg = "Active goal is not prunable" in
        Result.Error msg
