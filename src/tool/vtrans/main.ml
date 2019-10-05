open Core

let input_error_code = 1

let trans_error_code = 2

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

let translate file outdir () =
  match Ast.CoeusParselib.parse_file file with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      input_error_code
  | Result.Ok prog ->
      Logs.info (fun m -> m "Successfully parsed file %s" file) ;
      let frontend_config =
        let open Frontend.FrontendConfig in
        { fix_type= false
        ; simplify_arith= false
        ; lift_assume= true
        ; print= false }
      in
      match Frontend.Pipeline.run_default frontend_config prog with
      | Result.Error msg ->
          Logs.err (fun m -> m "%s" msg) ;
          input_error_code
      | Result.Ok prog ->
        try
          Translator.translate prog outdir ;
          0
        with Translator.TranslationError msg ->
          Logs.err (fun m -> m "Translation failure: %s" msg) ;
          trans_error_code

open Cmdliner

let input_error_info = Term.exit_info ~doc:"input errors" input_error_code

let trans_error_info =
  Term.exit_info ~doc:"translation errors" trans_error_code

let setup_log_arg =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let file_arg =
  let doc = "The input Coeus file." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let output_arg =
  let doc =
    "Path of the directory to store the output files. Default to the current \
     working directory"
  in
  Arg.(value & opt dir (Sys.getcwd ()) & info ["o"; "output"] ~docv:"DIR" ~doc)

let cmd =
  let doc = "Coeus-to-VeriMAPRel translator" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Translate a Coeus file into a format that can be recognized by the \
         VeriMapRel tool" ]
  in
  let exits = trans_error_info :: input_error_info :: Term.default_exits in
  ( Term.(const translate $ file_arg $ output_arg $ setup_log_arg)
  , Term.info "coeus-ctrans" ~version:"v0.1" ~doc ~exits ~man )

let () = Term.(exit_status (eval cmd))
