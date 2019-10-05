open Core
open Verifier
open Prover

let eval_load_session config file_path =
  let file_path =
    if String.is_prefix file_path "~" then
      match Sys.getenv "HOME" with
      | None -> file_path
      | Some home_expand -> home_expand ^ String.drop_prefix file_path 1
    else file_path
  in
  let precise_arith = ReplConfig.mem ~key:"frontend.precise_arith" config in
  SessionState.load_from_file ~precise_arith file_path

let eval_sides ~f side =
  let open ReplCommand in
  let open Result in
  let open Ast.Ecoeus in
  match side with
  | SideParam.OneSide s -> f s
  | SideParam.BothSides ->
      f Side.Left
      >>= fun lout ->
      f Side.Right
      >>= fun rout ->
      let line_sep = "--------" in
      let out =
        String.concat ~sep:"\n" [line_sep; lout; line_sep; rout; line_sep]
      in
      Result.Ok out

let eval_show_proc_one_side side _ (prover_state : ProverState.t) =
  let open Ast.Ecoeus in
  let ast = prover_state.ast in
  let left, right =
    match prover_state.goals with
    | [] -> (ast.entry.left, ast.entry.right)
    | goal :: _ -> (goal.left_proc, goal.right_proc)
  in
  let proc_name = match side with Side.Left -> left | Side.Right -> right in
  let proc = lookup_proc_exn ast proc_name in
  let out =
    Fmt.strf "%a" Ast.CoeusPrettyPrint.pp_proc (Procedure.to_coeus proc)
  in
  Result.Ok out

let eval_show_proc side config state =
  eval_sides side ~f:(fun s -> eval_show_proc_one_side s config state)

let eval_show_script _ state =
  let rule_history = SessionState.rule_history_of state in
  let rule_strs =
    String.concat ~sep:" ; " (List.map rule_history ~f:(fun r -> r.Rule.name))
  in
  Result.Ok rule_strs

let eval_show_pre _ Goal.({pre_conds; _}) =
  let rev_conds = List.rev pre_conds in
  let strs =
    List.map rev_conds ~f:(fun c -> Fmt.strf "@[<h 0>%a@]" Precondition.pp c)
  in
  let out = String.concat strs ~sep:".\n" in
  Result.Ok out

let eval_show_post _ Goal.({post_cond; _}) =
  let out = Fmt.strf "@[<h 0>%a@]" Postcondition.pp post_cond in
  Result.Ok out

let eval_show_blame _ Goal.({blame_step; _}) =
  let out =
    Fmt.strf "@[<h 0>blame = %a@]"
      (Fmt.option ~none:(fun fmt _ -> Fmt.string fmt "none") Fmt.int)
      blame_step
  in
  Result.Ok out

let eval_show_encode _ prover_state =
  let open Ast.Coeus in
  let dummy_config = ProverConfig.create_default () in
  let feature_vec = Prover.StateEncoder.encode dummy_config prover_state in
  let out =
    Fmt.strf "FeatureVec = @[<h>[%a]@]"
      (Fmt.array ~sep:Fmt.comma Fmt.float)
      feature_vec
  in
  Result.Ok out

let eval_show_actions _ prover_state =
  let open Ast.Coeus in
  let dummy_config = ProverConfig.create_default () in
  let action_states =
    ProverAction.applicable_candidate_rules_with_states dummy_config
      prover_state
  in
  let out =
    Fmt.strf "Available actions = [%a]"
      (Fmt.list ~sep:Fmt.comma Fmt.string)
      (List.map action_states ~f:(fun (r, _) -> r.Rule.name))
  in
  Result.Ok out

let eval_show_vc_smtlib _ ProverState.({verif_state; _}) =
  let sexps = Verifier.EldaricaEncoder.encode_sexp verif_state in
  let strs = List.map ~f:Sexp.to_string_hum sexps in
  let out = String.concat strs ~sep:"\n" in
  Result.Ok out

let eval_show_vc_muz _ ProverState.({verif_state; _}) =
  let decl_sexps, vc_sexps, query_sexp =
    Verifier.SpacerEncoder.encode_sexp verif_state
  in
  let sexps = List.concat [decl_sexps; vc_sexps; [query_sexp]] in
  let strs = List.map ~f:Sexp.to_string_hum sexps in
  let out = String.concat strs ~sep:"\n" in
  Result.Ok out

let eval_show_vc_houdini _ ProverState.({verif_state; _}) =
  let horn_ast = Solver.HornEncoder.encode verif_state in
  let out = Fmt.strf "%a" Solver.HornAst.pp horn_ast in
  Result.Ok out

let eval_show_vc_text _
    ProverState.({verif_state= VerifState.({pred_env; verif_conds; _}); _}) =
  let decls = PredEnv.signatures pred_env in
  let decl_strs =
    List.map decls ~f:(Fmt.strf "@[<h 0>%a@]" PredSignature.pp)
  in
  let decl_str = String.concat ~sep:"\n" decl_strs in
  let vc_strs =
    List.map verif_conds
      ~f:(fun VerifCondition.({pre_conds; post_cond; blame_step; _}) ->
        let post_str = Fmt.strf "@[<h 0>%a@]" Postcondition.pp post_cond in
        let pre_strs =
          List.map pre_conds ~f:(Fmt.strf "@[<h 0>%a@]" Precondition.pp)
        in
        let all_rev_strs =
          match blame_step with
          | None -> post_str :: pre_strs
          | Some i ->
              let blame_str = Fmt.strf "blame step %d" i in
              blame_str :: post_str :: pre_strs
        in
        let all_strs = List.rev all_rev_strs in
        String.concat ~sep:"\n" all_strs )
  in
  let all_strs = decl_str :: vc_strs in
  let line_sep = "********" in
  let sec_sep = Fmt.strf "\n%s\n" line_sep in
  let out =
    Fmt.strf "%s\n%s\n%s" line_sep
      (String.concat ~sep:sec_sep all_strs)
      line_sep
  in
  Result.Ok out

let eval_show_vc ~format =
  let open ReplCommand.VCFormat in
  match format with
  | Internal -> eval_show_vc_text
  | Smtlib -> eval_show_vc_smtlib
  | MuZ -> eval_show_vc_muz
  | Houdini -> eval_show_vc_houdini

let eval_peek_one_side side len _ Goal.({left_stmts; right_stmts; _}) =
  let open Ast.Ecoeus in
  let stmts =
    match side with Side.Left -> left_stmts | Side.Right -> right_stmts
  in
  let stmts' = List.take stmts len in
  let out =
    String.concat ~sep:"\n"
      (List.map stmts' ~f:(fun s -> Fmt.strf "@[<h 0>%a@]" Stmt.pp_brief s))
  in
  Result.Ok out

let eval_peek side len config goal =
  eval_sides side ~f:(fun s -> eval_peek_one_side s len config goal)

let eval_prover_add_history ~f config state =
  match SessionState.current_state_of state with
  | None ->
      let msg = "Cannot find any prover state in the current session" in
      Result.Error msg
  | Some prover_state ->
      let open Result in
      f config prover_state
      >>= fun (prover_state, rule) ->
      let state = SessionState.add_prover_state prover_state rule state in
      Result.Ok state

let eval_apply rule =
  eval_prover_add_history ~f:(fun _ state ->
      let open Result in
      ProverAction.apply_rule rule (ProverConfig.create_default ()) state
      >>= fun state' -> Ok (state', rule) )

let eval_applys rules config state =
  List.fold rules ~init:(Result.Ok state) ~f:(fun state_res rule ->
      match state_res with
      | Result.Error _ -> state_res
      | Result.Ok state -> eval_apply rule config state )

let get_solver_config config =
  let open Solver in
  let open Result in
  let find_timeout disabled key =
    if disabled then Ok Time.Span.zero
    else
      match ReplConfig.lookup_int config ~key with
      | None ->
          let msg = Fmt.strf "Required config key not found: %s" key in
          Error msg
      | Some i ->
          let span = Time.Span.of_sec (float_of_int i) in
          Ok span
  in
  let find_int_key disabled key =
    if disabled then Ok 0
    else
      match ReplConfig.lookup_int config ~key with
      | None ->
          let msg = Fmt.strf "Required config key not found: %s" key in
          Error msg
      | Some i -> Ok i
  in
  let disable_houdini = ReplConfig.mem config ~key:"houdini.disable" in
  let disable_spacer = ReplConfig.mem config ~key:"spacer.disable" in
  let disable_unsat_core = ReplConfig.mem config ~key:"unsat_core.disable" in
  find_timeout disable_houdini "houdini.timeout"
  >>= fun houdini_timeout ->
  find_int_key disable_houdini "houdini.max_guesses"
  >>= fun houdini_candidate_limit ->
  find_timeout disable_spacer "spacer.timeout"
  >>= fun spacer_timeout ->
  find_timeout (disable_spacer || disable_unsat_core) "unsat_core.timeout"
  >>= fun unsat_core_timeout ->
  let keep_tmp_file = ReplConfig.mem config ~key:"spacer.keep_tmp" in
  Ok
    SolverConfig.
      { disable_houdini
      ; disable_spacer
      ; disable_unsat_core
      ; houdini_timeout
      ; spacer_timeout
      ; unsat_core_timeout
      ; houdini_candidate_limit
      ; keep_tmp_file }

let eval_prune =
  eval_prover_add_history ~f:(fun config state ->
      let open Result in
      get_solver_config config
      >>= fun solver_config ->
      ProverAction.prune_goal solver_config state
      >>= fun state' -> Ok (state', ContraRule.contra) )

let eval_undo len _ state =
  let history = state.SessionState.history in
  let history_len = List.length history in
  (* Be careful not to drop the initial prover state *)
  if len >= history_len then
    let msg = Fmt.strf "Cannot unfo for more than %d steps" history_len in
    Result.Error msg
  else
    let history = List.drop history len in
    Result.Ok {state with history}

let eval_discharge config session_state =
  let open Result in
  match SessionState.current_state_of session_state with
  | None ->
      let msg = "No active prover state in the current session" in
      Error msg
  | Some prover_state -> (
      let open Solver in
      get_solver_config config
      >>= fun solver_config ->
      ProverAction.discharge solver_config prover_state
      >>= function
      | Status.Verified -> Result.Ok ()
      | Status.Rejected blame_steps ->
          let blame_msg =
            match blame_steps with
            | [] -> ""
            | _ ->
                let rule_history =
                  SessionState.rule_history_of session_state
                in
                let blamed_index_rules =
                  List.map blame_steps ~f:(fun s ->
                      let rule = List.nth_exn rule_history s in
                      (s, rule) )
                in
                let pp_blame_index_rule fmt (s, r) =
                  Fmt.pf fmt "%s at step %d" r.Rule.name s
                in
                Fmt.strf "\n[Blame] @[<h>%a@]"
                  (Fmt.list ~sep:Fmt.comma pp_blame_index_rule)
                  blamed_index_rules
          in
          let msg = Fmt.strf "Solver disproved the goals%s" blame_msg in
          Error msg
      | Status.Unknown ->
          let msg = "Solver returns unknown" in
          Error msg
      | Status.Timeout ->
          let msg = "Solver timed out" in
          Error msg )

let string_of_option key value =
  match value with "" -> key | _ -> Fmt.strf "%s = %s" key value

let eval_show_option keys config =
  let open Result in
  match keys with
  | [] ->
      let config_alist = ReplConfig.to_alist config in
      let config_strs =
        List.map config_alist ~f:(fun (key, value) ->
            string_of_option key value )
      in
      let out = String.concat config_strs ~sep:"\n" in
      Ok out
  | _ ->
      let show_one_option key =
        match ReplConfig.lookup ~key config with
        | None ->
            let msg = Fmt.strf "Cannot find option key \"%s\"" key in
            Error msg
        | Some value ->
            let out = string_of_option key value in
            Ok out
      in
      all (List.map keys ~f:show_one_option)
      >>= fun outs ->
      let out = String.concat outs ~sep:"\n" in
      Ok out

let eval_set_option alist config =
  let config' = ReplConfig.set_batch alist config in
  Result.Ok config'

let eval_unset_option keys config =
  let config' = ReplConfig.unset_batch ~keys config in
  Result.Ok config'

let result_map ~ok_f ~err_f = function
  | Result.Ok r -> ok_f r
  | Result.Error e -> err_f e

let eval_load (state : ReplState.t) path =
  eval_load_session state.config path
  |> result_map
       ~ok_f:(fun s ->
         let out =
           Fmt.strf "File \"%s\" loaded\nNew proof session created." path
         in
         ({state with session_state= Some s}, out) )
       ~err_f:(fun msg ->
         let msg = Fmt.strf "File loading failed: %s" msg in
         (state, msg) )

let extract_session_state ReplState.({session_state; config}) =
  match session_state with
  | Some s -> Result.Ok (config, s)
  | None ->
      let msg =
        Fmt.strf "Not in an active proof session. Please load a file first."
      in
      Result.Error msg

let eval_session_state ~f ~ok_f ~err_f state =
  let open Result in
  extract_session_state state
  >>= (fun (config, session_state) -> f config session_state)
  |> result_map ~ok_f ~err_f

let eval_prover_state ~f ~ok_f ~err_f state =
  eval_session_state state ~ok_f ~err_f ~f:(fun config session_state ->
      match SessionState.current_state_of session_state with
      | None ->
          let msg = "No active prover state in the current session" in
          Result.Error msg
      | Some prover_state -> f config prover_state )

let eval_goal_state ~f ~ok_f ~err_f state =
  eval_session_state state ~ok_f ~err_f ~f:(fun config session_state ->
      match SessionState.current_goal_of session_state with
      | None ->
          let msg = "No active goal in the current session" in
          Result.Error msg
      | Some goal -> f config goal )

let eval_option ~f ~ok_f ~err_f state =
  let config = state.ReplState.config in
  result_map (f config) ~ok_f ~err_f

let eval_command (state : ReplState.t) cmd : ReplState.t * string =
  let forward_msg msg = (state, msg) in
  let open ReplCommand in
  match cmd with
  | LoadFile path -> eval_load state path
  | ShowOption keys ->
      eval_option state ~f:(eval_show_option keys) ~ok_f:forward_msg
        ~err_f:forward_msg
  | SetOption alist ->
      eval_option state ~f:(eval_set_option alist)
        ~ok_f:(fun config -> ({state with config}, ""))
        ~err_f:forward_msg
  | UnsetOption keys ->
      eval_option state ~f:(eval_unset_option keys)
        ~ok_f:(fun config -> ({state with config}, ""))
        ~err_f:forward_msg
  | ShowProcedure side ->
      eval_prover_state state ~f:(eval_show_proc side) ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowPrecondition ->
      eval_goal_state state ~f:eval_show_pre ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowPostcondition ->
      eval_goal_state state ~f:eval_show_post ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowBlame ->
      eval_goal_state state ~f:eval_show_blame ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowEncode ->
      eval_prover_state state ~f:eval_show_encode ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowAvailableActions ->
      eval_prover_state state ~f:eval_show_actions ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowScript ->
      eval_session_state state ~f:eval_show_script ~ok_f:forward_msg
        ~err_f:forward_msg
  | ShowVerifCondition format ->
      eval_prover_state state ~f:(eval_show_vc ~format) ~ok_f:forward_msg
        ~err_f:forward_msg
  | PeekStmt {side; len} ->
      eval_goal_state state ~f:(eval_peek side len) ~ok_f:forward_msg
        ~err_f:forward_msg
  | ApplyRule rule ->
      eval_session_state state ~f:(eval_apply rule)
        ~ok_f:(fun s ->
          let out = "Rule applied" in
          ({state with session_state= Some s}, out) )
        ~err_f:forward_msg
  | ApplyRules rules ->
      eval_session_state state ~f:(eval_applys rules)
        ~ok_f:(fun s ->
          let out = "Rules applied" in
          ({state with session_state= Some s}, out) )
        ~err_f:forward_msg
  | PruneGoal ->
      eval_session_state state ~f:eval_prune
        ~ok_f:(fun s ->
          let out = "Active goal successfully pruned" in
          ({state with session_state= Some s}, out) )
        ~err_f:forward_msg
  | Undo len ->
      eval_session_state state ~f:(eval_undo len)
        ~ok_f:(fun s ->
          let out = Fmt.strf "Successfully undo %d steps" len in
          ({state with session_state= Some s}, out) )
        ~err_f:forward_msg
  | DischargeProof ->
      eval_session_state state ~f:eval_discharge
        ~ok_f:(fun _ ->
          let rule_history =
            SessionState.rule_history_of (Option.value_exn state.session_state)
          in
          let rule_strs =
            String.concat ~sep:" ; "
              (List.map rule_history ~f:(fun r -> r.Rule.name))
          in
          let out =
            Fmt.strf
              "Congratulations! Proof goals gets discharged successfully.\n\
               [Proof rules applied] %s\n\
               Proof session finished."
              rule_strs
          in
          ({state with session_state= None}, out) )
        ~err_f:forward_msg

let eval state command_s =
  match ReplCommand.from_string command_s with
  | None ->
      let out = Fmt.strf "Cannot parse command \"%s\"" command_s in
      (state, out)
  | Some command -> eval_command state command
