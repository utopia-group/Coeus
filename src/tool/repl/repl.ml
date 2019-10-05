open Core

let parse_config_string str =
  match String.split str ~on:':' with
  | [] -> None
  | [s] -> Some (s, "")
  | [k; v] -> Some (k, v)
  | _ ->
      let msg = Fmt.strf "Illegal config string format: \"%s\"" str in
      print_endline msg ; exit 1

let start_repl filename precise_arith disable_houdini houdini_timeout
    houdini_max_guess disable_spacer spacer_timeout disable_unsat_core
    unsat_core_timeout keep_tmp config_strs =
  (* Fmt_tty.setup_std_outputs () ;
   * Logs.set_level (Some Logs.Debug) ;
   * Logs.set_reporter (Logs_fmt.reporter ()) ; *)
  let config_alist = List.filter_map config_strs ~f:parse_config_string in
  let config_alist =
    if precise_arith then ("frontend.precise_arith", "") :: config_alist
    else config_alist
  in
  let config_alist =
    if disable_houdini then ("houdini.disable", "") :: config_alist
    else config_alist
  in
  let config_alist =
    if disable_spacer then ("spacer.disable", "") :: config_alist
    else config_alist
  in
  let config_alist =
    if disable_unsat_core then ("unsat_core.disable", "") :: config_alist
    else config_alist
  in
  let config_alist =
    ("houdini.max_guesses", string_of_int houdini_max_guess) :: config_alist
  in
  let config_alist =
    ("houdini.timeout", string_of_int houdini_timeout) :: config_alist
  in
  let config_alist =
    ("spacer.timeout", string_of_int spacer_timeout) :: config_alist
  in
  let config_alist =
    ("unsat_core.timeout", string_of_int unsat_core_timeout) :: config_alist
  in
  let config_alist =
    if keep_tmp then ("spacer.keep_tmp", "") :: config_alist else config_alist
  in
  let config = ReplConfig.of_alist config_alist in
  let empty_state = ReplState.{session_state= None; config} in
  let init_state =
    match filename with
    | "" -> empty_state
    | _ ->
        let command = ReplCommand.LoadFile filename in
        let new_state, out = ReplEvaluator.eval_command empty_state command in
        print_endline out ; new_state
  in
  ReplLoop.loop init_state

open Cmdliner

let filename_arg =
  let doc =
    "An input coeus file. If given, $(mname) will load it immediately after \
     startup."
  in
  Arg.(value & pos 0 file "" & info [] ~docv:"FILE" ~doc)

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

let config_arg =
  let doc =
    "Specify the additional config that will be set when loading the REPL. \
     User can specify multiple config strings. If the config string is of the \
     form \"key:value\", an additional \"set option key value\" command will \
     be executed. If there is no colon in the config string, the entire \
     string will be used as key and the value will be empty. This argument \
     overrides the config from all other command line arguments"
  in
  Arg.(value & opt_all string [] & info ["c"; "config"] ~docv:"CONFIG" ~doc)

let precise_arith_arg =
  let doc =
    "If specified, preserve the original semantics for all arithmetic \
     operations. Otherwise, non-linear arithmetic operations will be replaced \
     with uninterpreted functions as horn engines do not handle them very \
     well. "
  in
  let env = Arg.env_var "COEUS_PRECISE_ARITH" ~doc in
  Arg.(value & flag & info ["pa"; "precise-arith"] ~doc ~env)

let keep_tmp_arg =
  let doc =
    "Do not automatically remove the temporary smtlib files that are sent to \
     the external solver."
  in
  let env = Arg.env_var "COEUS_KEEP_TMP" ~doc in
  Arg.(value & flag & info ["k"; "keep-tmp"] ~doc ~env)

let cmd =
  let doc = "Coeus interactive REPL" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Fire a simple shell so that the user of the Coeus can interact with \
         the system." ]
  in
  ( (let open Term in
    const start_repl $ filename_arg $ precise_arith_arg $ disable_houdini_arg
    $ houdini_timeout_arg $ houdini_candiate_limit_arg $ disable_spacer_arg
    $ spacer_timeout_arg $ disable_unsat_core_arg $ unsat_core_timeout_arg
    $ keep_tmp_arg $ config_arg)
  , Term.info "coeus-repl" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man
  )

let () = Term.(exit @@ eval cmd)
