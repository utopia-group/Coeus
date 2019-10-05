open Cmdliner

let cmd =
  let doc = "advanced relational verification system" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = HelpCommand.help_secs in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info "coeus" ~version:"0.1" ~doc ~sdocs ~exits ~man )
