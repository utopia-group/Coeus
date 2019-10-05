open Core
open Prover

type t =
  { rules: Rule.t list
  ; failed_discharges: int
  ; num_blames: int
  ; timeout_discharges: int
  ; blocked_conflicts: int }
[@@deriving sexp, compare]

let json_of_rule rule =
  let open Yojson.Safe in
  let name = rule.Rule.name in
  `String name

let rule_of_json json =
  let open Yojson.Safe.Util in
  try
    let rule_name = to_string json in
    match RuleParselib.parse_rule rule_name with
    | Some r -> Result.Ok r
    | None ->
        let msg = Fmt.strf "Cannot parse rule \"%s\"" rule_name in
        Result.Error msg
  with Type_error (msg, _) ->
    let msg = Fmt.strf "Json error: %s" msg in
    Result.Error msg

let json_of
    { rules
    ; failed_discharges
    ; num_blames
    ; timeout_discharges
    ; blocked_conflicts } =
  let open Yojson.Safe in
  let assocs =
    [ ("rules", `List (List.map rules ~f:json_of_rule))
    ; ("failed_discharges", `Int failed_discharges)
    ; ("num_blames", `Int num_blames)
    ; ("timeout_discharges", `Int timeout_discharges)
    ; ("blocked_conflicts", `Int blocked_conflicts) ]
  in
  `Assoc assocs

let of_json json =
  let open Yojson.Safe.Util in
  let open Result in
  try
    let rules_json = json |> member "rules" |> to_list in
    let failed_discharges = json |> member "failed_discharges" |> to_int in
    let num_blames = json |> member "num_blames" |> to_int in
    let timeout_discharges = json |> member "timeout_discharges" |> to_int in
    let blocked_conflicts = json |> member "blocked_conflicts" |> to_int in
    all (List.map rules_json ~f:rule_of_json)
    >>= fun rules ->
    Ok
      { rules
      ; failed_discharges
      ; num_blames
      ; timeout_discharges
      ; blocked_conflicts }
  with Type_error (msg, _) ->
    let msg = Fmt.strf "Json error: %s" msg in
    Result.Error msg

let pp fmt res = Fmt.pf fmt "%a" (Yojson.pretty_print ~std:true) (json_of res)
