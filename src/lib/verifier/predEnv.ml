open Core
open Ast.Ecoeus

module SummaryType = struct
  type t = Precondition | Postcondition [@@deriving sexp, compare, hash]
end

module Source = struct
  type t =
    | Unknown
    | LoopInv of Stmt.t
    | RelLoopInv of Stmt.t * Stmt.t
    | ProcSummary of SummaryType.t * Procedure.t
    | RelProcSummary of SummaryType.t * Procedure.t * Procedure.t
  [@@deriving sexp, compare]
end

module Info = struct
  type t = {signature: PredSignature.t; source: Source.t}
  [@@deriving sexp, compare]
end

type t = Info.t Identifier.Map.t [@@deriving sexp, compare]

let create () = Identifier.Map.empty

let extend ?(source= Source.Unknown) env signature =
  let data = Info.{signature; source} in
  let key = signature.PredSignature.name in
  Identifier.Map.set env ~key ~data

let lookup = Identifier.Map.find

let signatures =
  Identifier.Map.fold ~init:[] ~f:(fun ~key:_ ~data acc ->
      let pred_sig = data.Info.signature in
      pred_sig :: acc )

let length = Identifier.Map.length

let map ~f env = List.map (Identifier.Map.data env) ~f

let rename ~f =
  Identifier.Map.fold ~init:Identifier.Map.empty ~f:(fun ~key ~data acc ->
      let key = f key in
      let signature =
        PredSignature.{name= key; params= data.Info.signature.params}
      in
      let data = Info.{signature; source= data.source} in
      Identifier.Map.add_exn acc ~key ~data )
