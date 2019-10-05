open Core

module SearchStrategy = struct
  type t =
    | Script
    | Random
    | SingleRolloutGuided
    | RepeatSingleRolloutGuided
    | SelfCompose
    | CompactSelfCompose
    | AlwaysSync
    | Descartes
    | SingleRolloutGuidedExhaustive
    | RandomGuidedExhaustive
    | DfsExhaustive
    | BfsExhaustive
end

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

let init_random_state opt_seed =
  let seed =
    Option.value opt_seed
      ~default:(Float.iround_towards_zero_exn (Unix.time ()))
  in
  Random.State.make [|seed|]

let init_solver disable_houdini houdini_timeout_sec houdini_candidate_limit
    disable_spacer spacer_timeout_sec disable_unsat_core unsat_core_timeout_sec
    keep_tmp_file =
  let open Solver in
  let houdini_timeout =
    Core.Time.Span.of_sec (float_of_int houdini_timeout_sec)
  in
  let spacer_timeout =
    Core.Time.Span.of_sec (float_of_int spacer_timeout_sec)
  in
  let unsat_core_timeout =
    Core.Time.Span.of_sec (float_of_int unsat_core_timeout_sec)
  in
  `Ok
    (let open SolverConfig in
    { disable_houdini
    ; houdini_timeout
    ; houdini_candidate_limit
    ; disable_spacer
    ; spacer_timeout
    ; disable_unsat_core
    ; unsat_core_timeout
    ; keep_tmp_file })

let get_inet_addr remote_addr =
  try
    let addr =
      match remote_addr with
      | None -> Unix.Inet_addr.bind_any
      | Some "localhost" -> Unix.Inet_addr.localhost
      | Some addr -> Unix.Inet_addr.of_string_or_getbyname addr
    in
    `Ok addr
  with Failure msg ->
    let msg = Fmt.strf "Hostname lookup error: %s" msg in
    `Error (false, msg)

open Cmdliner

module Command = struct
  type t = int Term.t * Term.info
end

let setup_log_term =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let seed_arg =
  let doc =
    "Random seed to use. If absent, use the current system time (in sec)."
  in
  let env = Arg.env_var "COEUS_SEED" ~doc in
  Arg.(value & opt (some int) None & info ["seed"] ~docv:"SEED" ~doc ~env)

let random_state_term = Term.(const init_random_state $ seed_arg)

let disable_houdini_arg =
  let doc = "Disable the Houdini solver. Default to false." in
  let open Arg in
  let env = env_var "COEUS_DISABLE_HOUDINI" ~doc in
  value & flag & info ["dh"; "disable-houdini"] ~doc ~env

let disable_spacer_arg =
  let doc = "Disable the Spacer solver. Default to false." in
  let open Arg in
  let env = env_var "COEUS_DISABLE_SPACER" ~doc in
  value & flag & info ["ds"; "disable-spacer"] ~doc ~env

let disable_unsat_core_arg =
  let doc =
    "Disable the unsat core solver. This is only meaningful if Spacer is \
     enabled. Default to false."
  in
  let open Arg in
  let env = env_var "COEUS_DISABLE_UNSAT_CORE" ~doc in
  value & flag & info ["du"; "disable-unsat-core"] ~doc ~env

let spacer_timeout_arg =
  let doc =
    "Specify the timeout of the spacer solver (in sec). Default to 10."
  in
  let open Arg in
  let env = env_var "COEUS_SPACER_TIMEOUT" ~doc in
  value & opt int 10 & info ["t"; "spacer-timeout"] ~docv:"TIMEOUT" ~doc ~env

let unsat_core_timeout_arg =
  let doc =
    "Specify the timeout of the unsat core solver (in sec). Default to 5."
  in
  let open Arg in
  let env = env_var "COEUS_UNSAT_CORE_TIMEOUT" ~doc in
  value & opt int 5
  & info ["ut"; "unsat-core-timeout"] ~docv:"TIMEOUT" ~doc ~env

let keep_tmp_arg =
  let doc = "Write the queries sent to to spacer to disk." in
  let open Arg in
  let env = env_var "COEUS_KEEP_TMP" ~doc in
  value & flag & info ["k"; "keep-tmp"] ~doc ~env

let houdini_timeout_arg =
  let doc =
    "Specify the timeout of the houdini solver (in sec). Default to 5."
  in
  let open Arg in
  let env = env_var "COEUS_HOUDINI_TIMEOUT" ~doc in
  value & opt int 5 & info ["ht"; "houdini-timeout"] ~docv:"TIMEOUT" ~doc ~env

let houdini_candiate_limit_arg =
  let doc =
    "Specify the max number of candidates that will be considered in the \
     Houdini algorithm. Default to 1000."
  in
  let open Arg in
  let env = env_var "COEUS_HOUDINI_MAX_GUESSES" ~doc in
  value & opt int 1000 & info ["hg"; "houdini-max-guess"] ~docv:"NUM" ~doc ~env

let setup_solver_term =
  let open Term in
  ret
    ( const init_solver $ disable_houdini_arg $ houdini_timeout_arg
    $ houdini_candiate_limit_arg $ disable_spacer_arg $ spacer_timeout_arg
    $ disable_unsat_core_arg $ unsat_core_timeout_arg $ keep_tmp_arg )

let precise_arith_arg =
  let doc =
    "If specified, preserve the original semantics for all arithmetic \
     operations. Otherwise, non-linear arithmetic operations will be replaced \
     by uninterpreted functions as horn engines do not handle them very well. "
  in
  let env = Arg.env_var "COEUS_PRECISE_ARITH" ~doc in
  Arg.(value & flag & info ["pa"; "precise-arith"] ~doc ~env)

let frontend_config_term =
  let create_config precise_arith =
    let simplify_arith = not precise_arith in
    let open Frontend.FrontendConfig in
    {simplify_arith; fix_type= false; lift_assume= false; print= false}
  in
  Term.(const create_config $ precise_arith_arg)

let strategy_arg =
  let alist =
    [ ("script", SearchStrategy.Script)
    ; ("random", SearchStrategy.Random)
    ; ("guided", SearchStrategy.SingleRolloutGuided)
    ; ("repeatguided", SearchStrategy.RepeatSingleRolloutGuided)
    ; ("selfcompose", SearchStrategy.SelfCompose)
    ; ("compactselfcompose", SearchStrategy.CompactSelfCompose)
    ; ("alwayssync", SearchStrategy.AlwaysSync)
    ; ("descartes", SearchStrategy.Descartes)
    ; ("guidedexhaustive", SearchStrategy.SingleRolloutGuidedExhaustive)
    ; ("randomguidedexhaustive", SearchStrategy.RandomGuidedExhaustive)
    ; ("dfsexhaustive", SearchStrategy.DfsExhaustive)
    ; ("bfsexhaustive", SearchStrategy.BfsExhaustive) ]
  in
  let doc =
    Fmt.strf
      "Proof strategy to use. Avaialble options: [%a]. Default to \"random\" \
       (one-shot random rollout)."
      (Fmt.list ~sep:Fmt.comma Fmt.string)
      (List.map alist ~f:(fun (name, _) -> Fmt.strf "\"%s\"" name))
  in
  let open Arg in
  value
  & opt (enum alist) SearchStrategy.Random
  & info ["proof"] ~docv:"STRATEGY" ~doc

let depth_limit_arg =
  let doc = "Depth limit of the search strategy. Default to unlimited." in
  let env = Arg.env_var "COEUS_DEPTH_LIMIT" ~doc in
  let open Arg in
  value
  & opt (some int) None
  & info ["d"; "depth"] ~docv:"DEPTH_LIMIT" ~doc ~env

let tactic_depth_limit_arg =
  let doc = "Depth limit of the search tactics. Default to unlimited." in
  let env = Arg.env_var "COEUS_TACTIC_DEPTH_LIMIT" ~doc in
  let open Arg in
  value
  & opt (some int) None
  & info ["td"; "tactic-depth"] ~docv:"TACTIC_DEPTH_LIMIT" ~doc ~env

let goal_limit_arg =
  let doc = "Goal limit of the search strategy. Default to unlimited." in
  let env = Arg.env_var "COEUS_GOAL_LIMIT" ~doc in
  let open Arg in
  value
  & opt (some int) None
  & info ["g"; "goals"] ~docv:"GOAL_LIMIT" ~doc ~env

let ast_size_limit_arg =
  let doc = "AST size limit of the prover states. Default to unlimited." in
  let env = Arg.env_var "COEUS_AST_SIZE_LIMIT" ~doc in
  let open Arg in
  value
  & opt (some int) None
  & info ["as"; "ast-size"] ~docv:"AST_SIZE_LIMIT" ~doc ~env

let memory_limit_arg =
  let doc =
    "Specify the size upper bound on memory usage (in MB). Default to \
     unlimited. Note that this option may add some overhead to the prover as \
     it needs to walk over the head repeatedly to obtain live memory size."
  in
  let env = Arg.env_var "COEUS_MEMORY_LIMIT" ~doc in
  let open Arg in
  value
  & opt (some int) None
  & info ["m"; "memory-limit"] ~docv:"MEMORY_LIMIT" ~doc ~env

let prover_config_term =
  let create_prover_config depth_limit tactic_depth_limit goal_limit
      ast_size_limit memory_limit_mb =
    let memory_limit =
      Option.map memory_limit_mb ~f:(fun limit -> limit * 1024 * 1024)
    in
    let open Prover.ProverConfig in
    {depth_limit; tactic_depth_limit; goal_limit; ast_size_limit; memory_limit}
  in
  let open Term in
  const create_prover_config $ depth_limit_arg $ tactic_depth_limit_arg
  $ goal_limit_arg $ ast_size_limit_arg $ memory_limit_arg

let max_conflict_arg =
  let doc =
    "Specify the max size of the conflict cache when using one of the \
     exhaustive strategy. If set to 0, conflict analysis during exhaustive \
     search will be disabled. Default to 0."
  in
  let env = Arg.env_var "COEUS_MAX_CONFLICT" ~doc in
  Arg.(value & opt int 0 & info ["mc"; "max-conflict"] ~docv:"SIZE" ~doc ~env)

let max_conflict_term =
  let check_size i =
    if i < 0 then
      let msg =
        Fmt.strf "Max conflict size is not a non-negative number: %d" i
      in
      `Error (false, msg)
    else `Ok i
  in
  Term.(ret (const check_size $ max_conflict_arg))

let search_config_term =
  let create_search_config prover_config solver_config max_conflict =
    Searcher.SearchConfig.{prover_config; solver_config; max_conflict}
  in
  let open Term in
  const create_search_config $ prover_config_term $ setup_solver_term
  $ max_conflict_term

let cache_file_arg =
  let doc =
    "Specify a cache file. $(mname) will cache results of solver queries into \
     this file. If the file does not exist, $(mname) will try to create a new \
     one. Otherwise, $(mname) will load its existing contents into the cache \
     firstread"
  in
  let env = Arg.env_var "COEUS_CACHE" ~doc in
  let open Arg in
  value & opt (some string) None & info ["c"; "cache"] ~docv:"CACHE" ~doc ~env

let cache_flush_freq_raw_arg =
  let doc =
    "The cache file, if exists, will be flushed after its size increases by \
     this number. Default to 50."
  in
  let env = Arg.env_var "COEUS_CACHE_FLUSH_FREQ" ~doc in
  Arg.(value & opt int 50 & info ["flush-freq"] ~docv:"SIZE" ~doc ~env)

let cache_flush_freq_arg =
  let check_freq i =
    if i <= 0 then
      let msg =
        Fmt.strf "Cache flush frequency is not a positive number: %d" i
      in
      `Error (false, msg)
    else `Ok i
  in
  Term.(ret (const check_freq $ cache_flush_freq_raw_arg))
