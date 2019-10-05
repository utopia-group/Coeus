open Core
open Ast.Coeus

module SideParam = struct
  type t = OneSide of Side.t | BothSides
end

module VCFormat = struct
  type t = Internal | Smtlib | MuZ | Houdini
end

type t =
  | LoadFile of string
  | DischargeProof
  | ShowOption of string list
  | SetOption of (string * string option) list
  | UnsetOption of string list
  | ShowProcedure of SideParam.t
  | ShowPrecondition
  | ShowPostcondition
  | ShowBlame
  | ShowScript
  | ShowEncode
  | ShowAvailableActions
  | ShowVerifCondition of VCFormat.t
  | PeekStmt of {side: SideParam.t; len: int}
  | ApplyRule of Prover.Rule.t
  | ApplyRules of Prover.Rule.t list
  | PruneGoal
  | Undo of int

let side_of_string = function
  | "l" | "left" -> Some (SideParam.OneSide Side.Left)
  | "r" | "right" -> Some (SideParam.OneSide Side.Right)
  | "b" | "both" -> Some SideParam.BothSides
  | _ -> None

let from_string s =
  let tokens = Str.(split (regexp "[ \t]+") s) in
  match tokens with
  | ("l" | "load") :: s :: _ -> Some (LoadFile s)
  | ("s" | "show") :: ("pre" | "precondition") :: _ -> Some ShowPrecondition
  | ("s" | "show") :: ("post" | "postcondition") :: _ -> Some ShowPostcondition
  | ("s" | "show") :: "blame" :: _ -> Some ShowBlame
  | ("s" | "show") :: ("e" | "encode") :: _ -> Some ShowEncode
  | ("s" | "show") :: ("a" | "actions") :: _ -> Some ShowAvailableActions
  | ("s" | "show") :: ("v" | "vc") :: rest ->
      let format =
        match List.hd rest with
        | Some ("s" | "smtlib") -> VCFormat.Smtlib
        | Some ("m" | "muz") -> VCFormat.MuZ
        | Some ("h" | "houdini") -> VCFormat.Houdini
        | _ -> VCFormat.Internal
      in
      Some (ShowVerifCondition format)
  | ("s" | "show") :: ("o" | "option") :: rest -> Some (ShowOption rest)
  | ("s" | "show") :: ("s" | "script") :: _ -> Some ShowScript
  | "set" :: ("o" | "option") :: key :: rest ->
      let value = List.hd rest in
      Some (SetOption [(key, value)])
  | "unset" :: ("o" | "option") :: key :: _ -> Some (UnsetOption [key])
  | "unset" :: "options" :: rest -> Some (UnsetOption rest)
  | ("s" | "show") :: side_s :: _ ->
      Option.(side_of_string side_s >>= fun s -> Some (ShowProcedure s))
  | ("p" | "peek") :: side_s :: rest ->
      let open Option in
      side_of_string side_s
      >>= fun side ->
      let len =
        match rest with
        | [] | ("a" | "all") :: _ -> Int.max_value
        | r :: _ -> match int_of_string_opt r with Some i -> i | None -> 0
      in
      Some (PeekStmt {side; len})
  | ("a" | "apply") :: apply_s :: _ ->
      let open Option in
      Prover.RuleParselib.parse_rule apply_s
      >>= fun rule -> Some (ApplyRule rule)
  | ("as" | "applys") :: (_ as rest) -> (
      let script = String.concat rest ~sep:" " in
      match Prover.RuleParselib.parse_rules script with
      | Result.Ok rules when not (List.is_empty rules) ->
          Some (ApplyRules rules)
      | _ -> None )
  | ("pr" | "prune") :: _ -> Some PruneGoal
  | ("d" | "discharge") :: _ -> Some DischargeProof
  | ("u " | "undo") :: rest ->
      let len =
        match rest with
        | [] -> 1
        | r :: _ -> match int_of_string_opt r with Some i -> i | None -> 0
      in
      Some (Undo len)
  | _ -> None
