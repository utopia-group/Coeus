val eval : ReplState.t -> string -> ReplState.t * string

val eval_command : ReplState.t -> ReplCommand.t -> ReplState.t * string
