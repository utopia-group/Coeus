type t =
  { rules: Prover.Rule.t list
  ; failed_discharges: int
  ; num_blames: int
  ; timeout_discharges: int
  ; blocked_conflicts: int }
[@@deriving sexp, compare]

val json_of : t -> Yojson.Safe.json

val of_json : Yojson.Safe.json -> (t, string) Core.Result.t

val pp : t Fmt.t
