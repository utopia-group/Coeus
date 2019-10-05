open Core

module MakeRequest (K : Sexpable.S) (V : Sexpable.S) = struct
  type t = Get of K.t | Set of K.t * V.t [@@deriving sexp]
end

module MakeResponse (V : Sexpable.S) = struct
  type t = Ack of int | Value of V.t option | Error of string
  [@@deriving sexp]
end
