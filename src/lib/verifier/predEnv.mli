open Ast.Ecoeus

module SummaryType : sig
  type t = Precondition | Postcondition [@@deriving sexp, compare, hash]
end

module Source : sig
  type t =
    | Unknown
    | LoopInv of Stmt.t
    | RelLoopInv of Stmt.t * Stmt.t
    | ProcSummary of SummaryType.t * Procedure.t
    | RelProcSummary of SummaryType.t * Procedure.t * Procedure.t
  [@@deriving sexp, compare]
end

module Info : sig
  type t = {signature: PredSignature.t; source: Source.t}
  [@@deriving sexp, compare]
end

type t [@@deriving sexp, compare]

val create : unit -> t

val extend : ?source:Source.t -> t -> PredSignature.t -> t

val lookup : t -> Identifier.t -> Info.t option

val length : t -> int

val signatures : t -> PredSignature.t list

val map : f:(Info.t -> 'a) -> t -> 'a list

val rename : f:(Identifier.t -> Identifier.t) -> t -> t
