open ProtocolMessage
open Core

module ServerStatus = struct
  type t = KeepConnection | TerminateConnection | TerminateServer
end

let min_cache_time = Time.Span.of_ms 200.0

let take_action state action_feature =
  let open Result in
  let open Prover in
  ServerState.get_current_session state
  >>= fun session ->
  ActionEncoder.rule_of_feature action_feature
  >>= fun rule ->
  Logs.info (fun m -> m "Taking action with rule %a" Rule.pp rule) ;
  let prover_config =
    let server_config = ServerState.get_config state in
    server_config.search_config.Searcher.SearchConfig.prover_config
  in
  RolloutSession.update prover_config rule session
  >>= fun session ->
  ServerState.update_current_session state session >>= fun _ -> Ok session

let get_reward server_state session =
  let open Result in
  let search_config =
    let config = ServerState.get_config server_state in
    config.Config.search_config
  in
  let open Searcher.ExhaustiveSearchState in
  match RolloutSession.try_discharge search_config session with
  | DischargeResult.Discharged _ ->
      Logs.info (fun m -> m "Solver succesfully discharged the goal") ;
      Ok 1.0
  | DischargeResult.(Timeout | NotDischarged) -> Ok 0.0
  | DischargeResult.Blamed blames ->
      ServerState.add_conflict server_state blames ;
      (* Having a blame might still be useful for us *)
      Ok 0.05

let respond_with_session server_state session =
  let open Prover in
  (* Logs.debug (fun m -> m "Current state: %a" ProverState.pp prover_state) ; *)
  let server_config = ServerState.get_config server_state in
  let prover_config =
    server_config.Config.search_config.Searcher.SearchConfig.prover_config
  in
  let prover_state = RolloutSession.prover_state_of session in
  match ProverState.is_fully_resolved prover_state with
  | false ->
      let state_features = StateEncoder.encode prover_config prover_state in
      let available_actions =
        (* TODO: Preserve the computed state somehow? *)
        let action_states =
          ProverAction.applicable_candidate_rule_indices_with_states
            prover_config prover_state
        in
        let action_states =
          List.filter action_states ~f:(fun (i, s) ->
              let rule = Rules.candidate_rules.(i) in
              if ServerState.check_conflict server_state rule s then (
                Logs.info (fun m ->
                    m "Conflict analysis blocked available rule %a" Rule.pp
                      rule ) ;
                false )
              else true )
        in
        List.map action_states ~f:(fun (i, _) -> i)
      in
      if List.is_empty available_actions then Response.Reward 0.0
      else
        let action_features = ActionEncoder.encode_actions available_actions in
        Response.NextState (state_features, action_features)
  | true ->
      Logs.info (fun m ->
          m "State is fully resolved. Making queries to the solvers now..." ) ;
      let bench_id =
        Option.value_exn (ServerState.get_current_benchmark_id server_state)
      in
      let rule_history = RolloutSession.history_of session in
      let action_history = List.map rule_history ~f:Rules.index_of_exn in
      let cache_key = (bench_id, action_history) in
      let resp =
        server_config.call_with_cache cache_key ~f:(fun _ ->
            let start_time = Time.now () in
            match get_reward server_state session with
            | Result.Ok reward ->
                let end_time = Time.now () in
                let duration = Time.diff end_time start_time in
                let should_cache = Time.Span.(duration >= min_cache_time) in
                (should_cache, Response.Reward reward)
            | Result.Error msg ->
                (* Swallow z3 crash as it happens a lot *)
                Logs.warn (fun m -> m "Goal discharging failed: %s" msg) ;
                (false, Response.Reward 0.0) )
      in
      resp

let handle_request state req =
  let open Request in
  let open Response in
  match req with
  | Ping -> Ack 0
  | TrainingBenchCount ->
      let config = ServerState.get_config state in
      let num = Array.length config.Config.training_benchs in
      Ack num
  | TestingBenchCount ->
      let config = ServerState.get_config state in
      let num = Array.length config.Config.testing_benchs in
      Ack num
  | FeatureCount ->
      let dummy_ast =
        let open Ast.Ecoeus in
        { procs= []
        ; entry= EntrySpec.default
        ; spec= Spec.{requires= []; ensures= []} }
      in
      let dummy_state =
        let open Prover.ProverState in
        { ast= dummy_ast
        ; depth= 0
        ; goals= []
        ; verif_state= Verifier.VerifState.empty }
      in
      let prover_config =
        let server_config = ServerState.get_config state in
        let search_config = server_config.search_config in
        search_config.prover_config
      in
      let features = Prover.StateEncoder.encode prover_config dummy_state in
      let num = Array.length features in
      Ack num
  | ActionCount ->
      let num = Array.length Prover.Rules.candidate_rules in
      Ack num
  | ActionName i -> (
    try
      let name = Prover.Rules.candidate_rule_names.(i) in
      AckString name
    with Invalid_argument msg -> Error msg )
  | BenchName i -> (
      let config = ServerState.get_config state in
      match Config.bench_of_id config i with
      | Some bench -> AckString bench
      | None ->
          let msg = Fmt.strf "Benchmark id out of range: %d" i in
          Error msg )
  | PickBench i -> (
    match ServerState.start_new_session state i with
    | Result.Error msg ->
        let msg = Fmt.strf "Benchmark initialization failed: %s" msg in
        Error msg
    | Result.Ok session -> respond_with_session state session )
  | RestartBench -> (
    match ServerState.restart_session state with
    | Result.Error msg ->
        let msg = Fmt.strf "RestartBench failed: %s" msg in
        Error msg
    | Result.Ok session -> respond_with_session state session )
  | TakeAction a -> (
    match take_action state a with
    | Result.Ok session -> respond_with_session state session
    | Result.Error msg ->
        Logs.warn (fun m -> m "Failed to take the specified action: %s" msg) ;
        Response.Reward 0.0 )
  | Stop | Quit ->
      failwith
        "[INTERNAL] handle_request should not see Stop and Quit commands"

let handle_single server_state in_chan out_chan =
  match Util.SocketIpc.read_remote_sexp ~of_sexp:Request.t_of_sexp in_chan with
  | Result.Ok Request.Quit ->
      Logs.info (fun m -> m "Quit command received. Shutdown server now.") ;
      ServerState.close_session server_state ;
      ServerStatus.TerminateServer
  | Result.Ok Request.Stop ->
      Logs.info (fun m -> m "Stop command received. Closing connection...") ;
      ServerState.close_session server_state ;
      ServerStatus.TerminateConnection
  | Result.Error msg ->
      let resp = Response.Error msg in
      Util.SocketIpc.write_remote_sexp ~sexp_of:Response.sexp_of_t out_chan
        resp ;
      ServerState.close_session server_state ;
      ServerStatus.TerminateConnection
  | Result.Ok req ->
      let resp = handle_request server_state req in
      Util.SocketIpc.write_remote_sexp ~sexp_of:Response.sexp_of_t out_chan
        resp ;
      ServerStatus.KeepConnection

let handle server_state in_chan out_chan =
  let rec loop () =
    match handle_single server_state in_chan out_chan with
    | ServerStatus.KeepConnection -> loop ()
    | ServerStatus.TerminateConnection -> true
    | ServerStatus.TerminateServer -> false
  in
  loop ()
