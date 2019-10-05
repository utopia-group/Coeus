open Core

type t =
  { depth_limit: int option
  ; tactic_depth_limit: int option
  ; goal_limit: int option
  ; ast_size_limit: int option
  ; memory_limit: int option }
[@@deriving sexp, compare, hash]

let create_default () =
  { depth_limit= None
  ; tactic_depth_limit= None
  ; goal_limit= None
  ; ast_size_limit= None
  ; memory_limit= None }
