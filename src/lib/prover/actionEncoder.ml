open Core

let rule_of_feature index =
  let num_actions = Array.length Rules.candidate_rules in
  if index >= 0 && index < num_actions then
    let rule = Rules.candidate_rules.(index) in
    Result.Ok rule
  else
    let msg =
      Fmt.strf
        "Action index out of bound: trying to select index %d from %d rules"
        index num_actions
    in
    Result.Error msg

let encode_action i = i

let encode_actions is = is
