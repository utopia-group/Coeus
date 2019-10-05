module SideParam : sig
  type t = OneSide of Ast.Coeus.Side.t | BothSides
end

module VCFormat : sig
  type t = Internal | Smtlib | MuZ | Houdini
end

type t =
  (* Session creation/destruction *)
  | LoadFile of string
  | DischargeProof
  (* Option manipulation *)
  | ShowOption of string list
  | SetOption of (string * string option) list
  | UnsetOption of string list
  (* State query *)
  | ShowProcedure of SideParam.t
  | ShowPrecondition
  | ShowPostcondition
  | ShowBlame
  | ShowScript
  | ShowEncode
  | ShowAvailableActions
  | ShowVerifCondition of VCFormat.t
  | PeekStmt of {side: SideParam.t; len: int}
  (* State manipluation *)
  | ApplyRule of Prover.Rule.t
  | ApplyRules of Prover.Rule.t list
  | PruneGoal
  (* History management *)
  | Undo of int

val from_string : string -> t option
