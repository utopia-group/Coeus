open Core
open Prover

module ProofStep = struct
  type t = {rule: Rule.t; prev_depth: int option; subgoal_index: int option}

  let pp fmt {rule; prev_depth; subgoal_index} =
    let pp_int_option =
      let pp_none fmt () = Fmt.string fmt "none" in
      Fmt.option ~none:pp_none Fmt.int
    in
    Fmt.pf fmt "(%a, %a, %a)" Rule.pp rule pp_int_option prev_depth
      pp_int_option subgoal_index
end

type t = {prover_state: ProverState.t; rev_history: ProofStep.t list}

let init prover_state = {prover_state; rev_history= []}

let to_rules {rev_history; _} =
  List.rev_map rev_history ~f:(fun ProofStep.({rule; _}) -> rule)

let history_of {rev_history; _} = List.rev rev_history

let pp_rules fmt state =
  let rules = to_rules state in
  Fmt.pf fmt "%a" Rule.pp_rules rules

let pp_history fmt state =
  let history = history_of state in
  Fmt.pf fmt "%a" (Fmt.list ~sep:Fmt.comma ProofStep.pp) history

module DischargeResult = struct
  type t =
    | Discharged of Rule.t list
    | Blamed of int list
    | Timeout
    | NotDischarged
end

let try_discharge search_config state =
  Logs.info (fun m -> m "Trying to discharge solution %a" pp_rules state) ;
  match
    ProverAction.discharge search_config.SearchConfig.solver_config
      state.prover_state
  with
  | Result.Ok Solver.Status.Verified ->
      let rules = to_rules state in
      DischargeResult.Discharged rules
  | Result.Ok Solver.Status.Timeout ->
      Logs.info (fun m -> m "Candidate solution timed out") ;
      DischargeResult.Timeout
  | Result.Ok (Solver.Status.Rejected blames) ->
      Logs.info (fun m -> m "Candidate solution rejected by solver") ;
      DischargeResult.Blamed blames
  | Result.Ok Solver.Status.Unknown ->
      Logs.info (fun m ->
          m "Candidate solution rejected by solver (reason unknown)" ) ;
      DischargeResult.NotDischarged
  | Result.Error msg ->
      Logs.info (fun m -> m "Candidate solution discharge error: %s" msg) ;
      DischargeResult.NotDischarged
