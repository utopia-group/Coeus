let error_code = 3

let fail_code = 4

let create_strategy sparam rnd_state opt_script opt_repeat remote_addr =
  let open Core in
  let open Searcher in
  let open CommandUtil in
  let sparam =
    match opt_script with Some _ -> SearchStrategy.Script | None -> sparam
  in
  match sparam with
  | SearchStrategy.Random ->
      let linear_strategy = RandomLinearStrategy.create rnd_state in
      Result.Ok (LinearStrategy.rollout linear_strategy)
  | SearchStrategy.SelfCompose ->
      let linear_strategy = SelfComposeStrategy.create () in
      Result.Ok (LinearStrategy.rollout linear_strategy)
  | SearchStrategy.CompactSelfCompose ->
      let linear_strategy = CompactSelfComposeStrategy.create () in
      Result.Ok (LinearStrategy.rollout linear_strategy)
  | SearchStrategy.AlwaysSync ->
      let linear_strategy = AlwaysSyncStrategy.create () in
      Result.Ok (LinearStrategy.rollout linear_strategy)
  | SearchStrategy.Descartes ->
      let strategy = DescartesStrategy.create () in
      Result.Ok strategy
  | SearchStrategy.DfsExhaustive ->
      let tree_strategy = RandomExhaustiveTreeStrategy.create rnd_state in
      Result.Ok (TreeStrategy.dfs tree_strategy)
  | SearchStrategy.BfsExhaustive ->
      let tree_strategy = RandomExhaustiveTreeStrategy.create rnd_state in
      Result.Ok (TreeStrategy.bfs tree_strategy)
  | SearchStrategy.SingleRolloutGuided ->
      Result.Ok (SingleRolloutGuidedStrategy.create rnd_state remote_addr)
  | SearchStrategy.RepeatSingleRolloutGuided ->
      Result.Ok
        (SingleRolloutGuidedStrategy.create_repeat ?count:opt_repeat rnd_state
           remote_addr)
  | SearchStrategy.SingleRolloutGuidedExhaustive ->
      Result.Ok (SingleRolloutGuidedExhaustiveStrategy.create remote_addr)
  | SearchStrategy.RandomGuidedExhaustive ->
      Result.Ok (SingleRolloutGuidedExhaustiveStrategy.create_random rnd_state)
  | SearchStrategy.Script -> (
    match opt_script with
    | None ->
        let msg = Fmt.strf "Missing script file in proof script mode" in
        Result.Error msg
    | Some path ->
        let content = In_channel.read_all path in
        let open Result in
        Prover.RuleParselib.parse_rules content
        >>= fun rules ->
        let linear_strategy = FixedRulesStrategy.create rules in
        Ok (LinearStrategy.rollout linear_strategy) )

let run filename repeat output_json sparam opt_script rnd_state frontend_config
    search_config addr port () =
  let open Core in
  match Ast.CoeusParselib.parse_file filename with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      `Ok ParseCommand.error_code
  | Result.Ok prog -> (
    match Frontend.Pipeline.run_default frontend_config prog with
    | Result.Error msg ->
        Logs.err (fun m -> m "%s" msg) ;
        `Ok CheckCommand.error_code
    | Result.Ok ast -> (
      match
        create_strategy sparam rnd_state opt_script repeat (addr, port)
      with
      | Result.Error msg ->
          Logs.app (fun m -> m "Proof strategy creation failed: %s" msg) ;
          `Ok error_code
      | Result.Ok strategy -> (
          let open Prover in
          let init_state = ProverState.init ast in
          let start_time = Time.now () in
          match strategy search_config init_state with
          | Result.Error msg ->
              Logs.app (fun m -> m "Proof strategy failed: %s" msg) ;
              `Ok fail_code
          | Result.Ok search_result ->
              let end_time = Time.now () in
              let duration = Time.diff end_time start_time in
              let duration_sec = Time.Span.to_sec duration in
              if output_json then
                let result_json =
                  Searcher.SearchResults.SearchStatus.json_of_succ duration_sec
                    search_result
                in
                Logs.app (fun m ->
                    m "%a" (Yojson.Safe.pretty_print ~std:true) result_json )
              else (
                Logs.app (fun m ->
                    m "Proof strategy succeeded in %f seconds." duration_sec ) ;
                Logs.app (fun m ->
                    m "Applied rules: %a"
                      (Fmt.list ~sep:Fmt.comma Fmt.string)
                      (List.map ~f:(fun r -> r.Rule.name) search_result.rules)
                ) ) ;
              `Ok 0 ) ) )

open Cmdliner

let error_info = Term.exit_info ~doc:"run errors" error_code

let fail_info = Term.exit_info ~doc:"proof failure" fail_code

let script_arg =
  let doc =
    "Path to the proof script. If this parameter is presented, search \
     strategy will be force-set to \"script\"."
  in
  let env = Arg.env_var "COEUS_SCRIPT" ~doc in
  let open Arg in
  value & opt (some file) None & info ["s"; "script"] ~docv:"SCRIPT" ~doc ~env

let repeat_arg =
  let doc =
    "Specify the number of repetitions in a multiple rollout strategy. This \
     parameter is meaningful only when the search strategy is set to \
     \"repeatguided\". By default, no bound will be set for the number of \
     repetitions. "
  in
  let env = Arg.env_var "COEUS_REPEAT" ~doc in
  let open Arg in
  value & opt (some int) None & info ["r"; "repeat"] ~docv:"N" ~doc ~env

let json_arg =
  let doc = "Print search result in JSON format instead of texts." in
  let env = Arg.env_var "COEUS_OUTPUT_JSON" ~doc in
  Arg.(value & flag & info ["j"; "output-json"] ~doc ~env)

let remoteaddr_arg =
  let doc =
    "Specify which remote address the client will connect to. Default to \
     localhost."
  in
  let env = Arg.env_var "COEUS_REMOTE_ADDR" ~doc in
  let open Arg in
  value & opt (some string) None & info ["a"; "addr"] ~docv:"ADDR" ~doc ~env

let remoteaddr_term =
  Term.(ret (const CommandUtil.get_inet_addr $ remoteaddr_arg))

let remoteport_arg =
  let doc =
    "Specify the remote port the client will connect to. Default to 12345"
  in
  let env = Arg.env_var "COEUS_REMOTE_PORT" ~doc in
  Arg.(value & opt int 12345 & info ["p"; "port"] ~docv:"PORT" ~doc ~env)

let cmd =
  let doc = "run proof strategies on the input file" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Parse and type check the input source file, then apply a specific \
         proof strategy to it." ]
  in
  let exits =
    fail_info :: error_info :: CheckCommand.error_info
    :: ParseCommand.error_info :: Term.default_exits
  in
  ( (let open Term in
    ret
      ( const run $ ParseCommand.filename_arg $ repeat_arg $ json_arg
      $ CommandUtil.strategy_arg $ script_arg $ CommandUtil.random_state_term
      $ CommandUtil.frontend_config_term $ CommandUtil.search_config_term
      $ remoteaddr_term $ remoteport_arg $ CommandUtil.setup_log_term ))
  , Term.info "run" ~doc ~sdocs ~man ~exits )
