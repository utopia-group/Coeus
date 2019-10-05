open Core

module StateFeature = struct
  type t = float array [@@deriving sexp, compare]
end

module ProbDistribution = struct
  type t = float array [@@deriving sexp, compare]
end

module ActionFeature = struct
  type t = int [@@deriving sexp, compare, hash]
end

module ActionList = struct
  type t = ActionFeature.t list [@@deriving sexp, compare, hash]
end
