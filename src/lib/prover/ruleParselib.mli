val parse_rule : string -> Rule.t option

val parse_rules : string -> (Rule.t list, string) Core.Result.t
