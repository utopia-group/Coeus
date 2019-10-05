let cmds =
  [ ParseCommand.cmd
  ; CheckCommand.cmd
  ; RunCommand.cmd
  ; ServerCommand.cmd
  ; ReplayCommand.cmd
  ; HelpCommand.cmd ]

let () = Cmdliner.Term.(exit_status (eval_choice DefaultCommand.cmd cmds))
