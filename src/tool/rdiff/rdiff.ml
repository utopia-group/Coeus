open Cmdliner

let default_cmd =
  let doc = "Coeus search result diff tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_common_options
    ; `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command." ]
  in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))
  , Term.info "rdiff" ~version:"0.1" ~doc ~sdocs ~exits ~man )

let cmds = [StatusCommand.cmd; TimeCommand.cmd]

let _ = Term.(exit @@ eval_choice default_cmd cmds)
