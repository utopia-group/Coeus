open Core
open Util.Feature

module Request = struct
  type t =
    | Ping
    | TrainingBenchCount
    | TestingBenchCount
    | FeatureCount
    | ActionCount
    | BenchName of int
    | ActionName of ActionFeature.t
    | PickBench of int
    | RestartBench
    | TakeAction of ActionFeature.t
    | Stop
    | Quit
  [@@deriving sexp, compare]
end

module Response = struct
  type t =
    | Ack of int
    | AckString of string
    | NextState of StateFeature.t * ActionList.t
    | Reward of float
    | Error of string
  [@@deriving sexp, compare]
end
