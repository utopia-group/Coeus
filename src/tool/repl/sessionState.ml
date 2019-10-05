open Core

module HistoryState = struct
  type t = {prover_state: Prover.ProverState.t; prev_rule: Prover.Rule.t option}
end

type t = {file_path: string; ast: Ast.Ecoeus.t; history: HistoryState.t list}

let load_from_file ?(precise_arith= false) file_path =
  let open Result in
  Ast.CoeusParselib.parse_file file_path
  >>= fun prog ->
  let frontend_config =
    let open Frontend.FrontendConfig in
    { fix_type= false
    ; print= false
    ; lift_assume= false
    ; simplify_arith= not precise_arith }
  in
  Frontend.Pipeline.run_default frontend_config prog
  >>= fun ast ->
  let open Prover in
  let prover_state = ProverState.init ast in
  let history_state = HistoryState.{prover_state; prev_rule= None} in
  Ok {file_path; ast; history= [history_state]}

let add_prover_state prover_state rule state =
  let history_state = HistoryState.{prover_state; prev_rule= Some rule} in
  let history = history_state :: state.history in
  {state with history}

let current_state_of {history; _} =
  let open Option in
  List.hd history >>= fun HistoryState.({prover_state; _}) -> Some prover_state

let current_goal_of state =
  let open Option in
  current_state_of state
  >>= fun prover_state -> Prover.ProverState.current_goal_of prover_state

let rule_history_of {history; _} =
  let rev_rules =
    List.filter_map history ~f:(fun HistoryState.({prev_rule; _}) -> prev_rule)
  in
  List.rev rev_rules
