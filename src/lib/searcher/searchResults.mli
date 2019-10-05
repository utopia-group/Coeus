module SearchStatus : sig
  type t = Fail | Timeout | Succ of float * SearchResult.t
  [@@deriving sexp, compare]

  val json_of_succ : float -> SearchResult.t -> Yojson.Safe.json

  val json_of : t -> Yojson.Safe.json

  val of_json : Yojson.Safe.json -> (t, string) Core.Result.t

  val pp : t Fmt.t
end

type t = SearchStatus.t Core.String.Map.t

val json_of : t -> Yojson.Safe.json

val save : string -> t -> unit

val load : string -> (t, string) Core.Result.t
