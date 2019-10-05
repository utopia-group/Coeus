val error_code : int

val error_info : Cmdliner.Term.exit_info

val filename_arg : string Cmdliner.Term.t

val print_arg : bool Cmdliner.Term.t

val cmd : CommandUtil.Command.t
