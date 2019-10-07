open Core

(* Ideally the rule parser should just be a hashmap from names to rules. Having
a dedicated rule lexer and rule parser is mostly due to historical reason: In
earlier prototype, rules can take parameters of various types, which would
justfiy a more complicated parsing logic. Unfortunately, parameterized actions
do not play well with reinforcement learning, which is why we have removed them
in the current revision. *)

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
