open Core
open Prover
open Solver

type t =
  { prover_config: ProverConfig.t
  ; solver_config: SolverConfig.t
  ; max_conflict: int }

let is_target_state {solver_config; _} state =
  ProverAction.is_state_provable solver_config state
