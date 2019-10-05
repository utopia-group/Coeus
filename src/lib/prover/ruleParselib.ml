open Core

let parse_rule s =
  let lexbuf = Lexing.from_string s in
  try Some (RuleParser.main RuleLexer.lex lexbuf) with
  | RuleLexer.LexingError _ | RuleParser.Error -> None

let parse_rules s =
  let rule_strs =
    List.map (String.split ~on:';' s) ~f:(fun s -> String.strip s)
  in
  Result.all
    (List.map rule_strs ~f:(fun s ->
         match parse_rule s with
         | Some rule -> Ok rule
         | None ->
             let msg = Fmt.strf "Cannot parse rule \"%s\"" s in
             Error msg ))
