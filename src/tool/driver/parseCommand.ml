open Cmdliner

let error_code = 1

let error_info = Term.exit_info ~doc:"parser errors" error_code

let filename_arg =
  let doc =
    "Input coeus file. If not given, $(mname) will read the input from stdin."
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let print_arg =
  let doc = "Pretty-print the parsed coeus file to stdin" in
  Arg.(value & flag & info ["p"; "print"] ~doc)

let parse filename print_flag () =
  match Ast.CoeusParselib.parse_file filename with
  | Result.Ok prog ->
      if print_flag then Ast.CoeusPrettyPrint.pp Fmt.stdout prog
      else Logs.app (fun m -> m "Successfully parsed input file %s" filename) ;
      `Ok 0
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      `Ok error_code

let cmd =
  let doc = "parse the input file" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P "Parse the input source file using the Coeus grammar." ]
  in
  let exits = error_info :: Term.default_exits in
  ( (let open Term in
    ret (const parse $ filename_arg $ print_arg $ CommandUtil.setup_log_term))
  , Term.info "parse" ~doc ~sdocs ~man ~exits )
