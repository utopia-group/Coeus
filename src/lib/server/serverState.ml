open Core
open Searcher

type t =
  { server_config: Config.t
  ; conflict_cache: ConflictAnalysis.ConflictCache.t
  ; curr_id: int option ref
  ; curr_session: RolloutSession.t option ref }

let init server_config =
  let conflict_size =
    server_config.Config.search_config.SearchConfig.max_conflict
  in
  let conflict_cache =
    ConflictAnalysis.ConflictCache.create ~size:conflict_size
  in
  {server_config; conflict_cache; curr_id= ref None; curr_session= ref None}

let do_start_session {server_config; curr_session; curr_id; _} bench_id =
  let open Result in
  match Config.bench_of_id server_config bench_id with
  | None ->
      let msg = Fmt.strf "Benchmark index out of range: %d" bench_id in
      Error msg
  | Some bench ->
      Logs.info (fun m -> m "Starting new session on benchmark \"%s\"" bench) ;
      RolloutSession.load_from_file server_config.frontend_config bench
      >>= fun new_session ->
      curr_id := Some bench_id ;
      curr_session := Some new_session ;
      Ok new_session

let start_new_session server_state bench_id =
  let open Result in
  do_start_session server_state bench_id
  >>= fun session ->
  ConflictAnalysis.ConflictCache.clear server_state.conflict_cache ;
  Result.Ok session

let restart_session server_state =
  match !(server_state.curr_id) with
  | None ->
      let msg = "No active session to restart" in
      Result.Error msg
  | Some id ->
      (* Restart does not clear conflict cache *)
      do_start_session server_state id

let close_session {curr_session; curr_id; _} =
  curr_id := None ;
  curr_session := None

let get_current_benchmark_id {curr_id; _} = !curr_id

let get_current_session {curr_session; _} =
  match !curr_session with
  | None -> Result.Error "Active session not found"
  | Some s -> Result.Ok s

let update_current_session {curr_session; _} new_session =
  match !curr_session with
  | None -> Result.Error "Active session not found"
  | Some _ ->
      curr_session := Some new_session ;
      Result.Ok ()

let add_conflict {conflict_cache; server_config; curr_session; _} blames =
  match !curr_session with
  | None -> ()
  | Some session ->
      let max_conflict =
        server_config.Config.search_config.SearchConfig.max_conflict
      in
      let search_state = RolloutSession.search_state_of session in
      if max_conflict > 0 && not (List.is_empty blames) then
        ConflictAnalysis.add_conflict conflict_cache search_state blames

let check_conflict {conflict_cache; curr_session; _} rule new_state =
  match !curr_session with
  | None -> false
  | Some session -> (
      let open Prover in
      let search_state = RolloutSession.search_state_of session in
      match
        ProverState.current_goal_of (RolloutSession.prover_state_of session)
      with
      | None -> false
      | Some goal ->
          let prev_depth = goal.Goal.prev_step in
          let subgoal_index = goal.subgoal_index in
          let step =
            ExhaustiveSearchState.ProofStep.{rule; prev_depth; subgoal_index}
          in
          let rev_history = step :: search_state.rev_history in
          let search_state =
            ExhaustiveSearchState.{prover_state= new_state; rev_history}
          in
          ConflictAnalysis.check_conflict conflict_cache search_state )

let get_config {server_config; _} = server_config
