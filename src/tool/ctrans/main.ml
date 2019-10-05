open Core
open Ctranslator

let error_code = 1

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

let init_trans_config print_c print_csyntax disable_retype_vars type_check
    fix_types =
  let open TransConfig in
  { print_c
  ; print_csyntax
  ; retype_vars= not disable_retype_vars
  ; type_check
  ; fix_types }

let translate file0 file1 trans_config () =
  Run.init () ;
  match Run.translate_pair trans_config file0 file1 with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      `Ok error_code
  | Result.Ok prog ->
      Ast.CoeusPrettyPrint.pp Fmt.stdout prog ;
      `Ok 0

open Cmdliner

let error_info = Term.exit_info ~doc:"translation errors" error_code

let filename0_arg =
  let doc = "The first input C file." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE0" ~doc)

let filename1_arg =
  let doc = "The second input C file" in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE1" ~doc)

let print_c_arg =
  let doc = "Print parsed C AST to stderr" in
  Arg.(value & flag & info ["print-c"] ~doc)

let type_check_arg =
  let doc = "Type check the translated program" in
  Arg.(value & flag & info ["t"; "type-check"] ~doc)

let fix_type_arg =
  let doc =
    "Try to insert casts into the translated program to make it type-check at \
     the Coeus level. This happens before type check."
  in
  Arg.(value & flag & info ["f"; "fix-type"] ~doc)

let disable_retype_var_arg =
  let doc =
    "By default, the translator will change the type of all variables whose \
     names start with \"bool\" to bool type. This is useful for programs \
     obtained by our random sampler but not for other programs. Use this flag \
     to explicitly request the translator to disable the retyping behavior."
  in
  Arg.(value & flag & info ["disable-retype"] ~doc)

let print_cs_arg =
  let doc = "Print parsed CSyntax AST to stderr" in
  Arg.(value & flag & info ["print-cs"] ~doc)

let setup_trans_arg =
  let open Term in
  const init_trans_config $ print_c_arg $ print_cs_arg $ disable_retype_var_arg
  $ type_check_arg $ fix_type_arg

let setup_log_arg =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "C-to-Coeus translator" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Translate two C files into a single Coeus file, assuming each C file \
         only contains one function" ]
  in
  let exits = error_info :: Term.default_exits in
  ( (let open Term in
    const translate $ filename0_arg $ filename1_arg $ setup_trans_arg
    $ setup_log_arg)
  , Term.info "coeus-ctrans" ~version:"v0.1" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
