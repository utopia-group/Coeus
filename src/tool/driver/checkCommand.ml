open Core

let error_code = 2

let check filename config () =
  match Ast.CoeusParselib.parse_file filename with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      `Ok ParseCommand.error_code
  | Result.Ok prog ->
    match Frontend.Pipeline.run_default config prog with
    | Result.Error msg ->
        Logs.err (fun m -> m "%s" msg) ;
        `Ok error_code
    | Result.Ok _ ->
        if not config.print then
          Logs.app (fun m ->
              m "Successfully type checked input file %s" filename ) ;
        `Ok 0

open Cmdliner

let error_info = Term.exit_info ~doc:"checker errors" error_code

let fix_type_arg =
  let doc = "Try fixing simple type errors before performing type checks" in
  Arg.(value & flag & info ["f"; "fix-type"] ~doc)

let simplify_arith_arg =
  (* We don't use CommandUtil.simplify_arith_arg here, as we want the frontend behavior to be more predictable: no additional transformations should apply unless the user explicitly request for them *)
  let doc =
    "If specified, non-linear arithmetic operations will be replaced with \
     uninterpreted functions. "
  in
  Arg.(value & flag & info ["sa"; "simplify-arith"] ~doc)

let lift_assume_arg =
  let doc =
    "If specified, lift all assume statements that appear at the beginning of \
     the entry methods to the relational specifications."
  in
  Arg.(value & flag & info ["la"; "lift-assume"] ~doc)

let frontend_config_term =
  let create_config fix_type simplify_arith lift_assume print =
    Frontend.FrontendConfig.{fix_type; simplify_arith; lift_assume; print}
  in
  let open Term in
  const create_config $ fix_type_arg $ simplify_arith_arg $ lift_assume_arg
  $ ParseCommand.print_arg

let cmd =
  let doc = "type check the input file" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Parse the input source file using the Coeus grammar and then check \
         its type consistency." ]
  in
  let exits = error_info :: ParseCommand.error_info :: Term.default_exits in
  ( (let open Term in
    ret
      ( const check $ ParseCommand.filename_arg $ frontend_config_term
      $ CommandUtil.setup_log_term ))
  , Term.info "check" ~doc ~sdocs ~man ~exits )
