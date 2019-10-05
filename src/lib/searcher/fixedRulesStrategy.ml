open Core
open Ast.Coeus

let pick_rule_from rules =
  let remaining_rules = ref rules in
  fun _ _ ->
    match !remaining_rules with
    | [] -> None
    | r :: rest ->
        remaining_rules := rest ;
        Some r

let create rules = RulePicker.to_linear_strategy (pick_rule_from rules)
