open Core

let error_code = 7

let find_script opt_file opt_str =
  let parse_rules str =
    match Prover.RuleParselib.parse_rules str with
    | Result.Ok rules -> `Ok rules
    | Result.Error msg ->
        Logs.err (fun m -> m "%s" msg) ;
        `Error (false, "Rule parsing error")
  in
  match (opt_file, opt_str) with
  | None, None ->
      let msg = "Proof script not specified" in
      `Error (false, msg)
  | Some _, Some _ ->
      let msg =
        "Proof script can come from either a file or a string but not both"
      in
      `Error (false, msg)
  | Some file, None ->
      let content = In_channel.read_all file in
      parse_rules content
  | None, Some str -> parse_rules str

let print_trace =
  List.iter ~f:(fun (state_features, action_feature, masks) ->
      Logs.app (fun m ->
          m "@[<h>%a ; %d; %a@]"
            (Fmt.array ~sep:Fmt.sp Fmt.float)
            state_features action_feature
            (Fmt.list ~sep:Fmt.sp Fmt.int)
            masks ) )

let get_rule_index rule =
  let open Prover in
  let open Option in
  let rule_name = rule.Rule.name in
  Array.findi Rules.candidate_rule_names (fun _ name ->
      String.equal name rule_name )
  >>= fun (idx, _) -> Some idx

let do_replay rules prover_config init_state =
  let open Ast.Ecoeus in
  let open Prover in
  let rec run_impl acc state = function
    | [] ->
        if ProverState.is_fully_resolved state then
          (* Successfully resolved all proof goals *)
          Result.Ok (List.rev acc)
        else
          let msg =
            Fmt.strf
              "Proof script is exhausted but some proof goals are still not \
               resolved"
          in
          Result.Error msg
    | rule :: rest -> (
      match get_rule_index rule with
      | None ->
          let msg =
            Fmt.strf "Cannot find index for rule \"%a\"" Rule.pp rule
          in
          Result.Error msg
      | Some rule_idx -> (
          let state_features = StateEncoder.encode prover_config state in
          let action_feature = ActionEncoder.encode_action rule_idx in
          let available_actions =
            let rule_index_states =
              ProverAction.applicable_candidate_rule_indices_with_states
                prover_config state
            in
            List.map rule_index_states ~f:(fun (i, _) -> i)
          in
          let acc =
            (state_features, action_feature, available_actions) :: acc
          in
          match ProverAction.apply_rule rule prover_config state with
          | Result.Error msg ->
              let msg =
                Fmt.strf "Rule \"%a\" application error: %s" Rule.pp rule msg
              in
              Result.Error msg
          | Result.Ok new_state -> run_impl acc new_state rest ) )
  in
  run_impl [] init_state rules

let replay () file rules frontend_config prover_config =
  match Ast.CoeusParselib.parse_file file with
  | Result.Error msg ->
      Logs.err (fun m -> m "Input parsing error: %s\n" msg) ;
      ParseCommand.error_code
  | Result.Ok prog -> (
    match Frontend.Pipeline.run_default frontend_config prog with
    | Result.Error msg ->
        Logs.err (fun m -> m "Type checking error: %s\n" msg) ;
        CheckCommand.error_code
    | Result.Ok ast -> (
        let init_state = Prover.ProverState.init ast in
        match do_replay rules prover_config init_state with
        | Result.Ok trace -> print_trace trace ; 0
        | Result.Error msg ->
            Logs.err (fun m -> m "Proof script application failed: %s" msg) ;
            error_code ) )

open Cmdliner

let error_info = Term.exit_info ~doc:"replay errors" error_code

let script_file_arg =
  let doc =
    "Path to the proof script, if the script should be read from a file"
  in
  let open Arg in
  value
  & opt (some file) None
  & info ["f"; "script-file"] ~docv:"SCRIPT_FILE" ~doc

let script_string_arg =
  let doc =
    "Content of the proof script, if the script should be read from a string"
  in
  let open Arg in
  value
  & opt (some string) None
  & info ["s"; "script-str"] ~docv:"SCRIPT_STR" ~doc

let script_term =
  Term.(ret (const find_script $ script_file_arg $ script_string_arg))

let cmd =
  let doc = "Coeus script replay tool" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Replay a proof on a Coeus program and dump the intermediate \
         state/action encoding."
    ; `P
        "Output format for each line: [state encoding] ; [action]; [available \
         actions]" ]
  in
  let exits =
    error_info :: CheckCommand.error_info :: ParseCommand.error_info
    :: Term.default_exits
  in
  ( (let open Term in
    const replay $ CommandUtil.setup_log_term $ ParseCommand.filename_arg
    $ script_term $ CommandUtil.frontend_config_term
    $ CommandUtil.prover_config_term)
  , Term.info "replay" ~version:"v0.1" ~doc ~exits ~man )
