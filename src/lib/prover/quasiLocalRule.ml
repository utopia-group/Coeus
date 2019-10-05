open Core
open Verifier
module Ecoeus = Ast.Ecoeus

module IsApplicable = struct
  type t = Ecoeus.t -> Goal.t -> bool
end

type t =
     Ecoeus.t
  -> PredEnv.t
  -> int
  -> Goal.t
  -> (Goal.t list * PredEnv.t * Ecoeus.t) option

let mk_rule ?is_applicable ?(is_aggressive= false) ~name (quasilocal_rule: t) =
  let apply _ (state: ProverState.t) =
    let verif_state = state.verif_state in
    match state.goals with
    | [] -> Result.Error "empty active goal"
    | goal :: rest ->
      match
        quasilocal_rule state.ast verif_state.pred_env state.depth goal
      with
      | None -> Result.Error "rule not applicable"
      | Some (new_goals, new_env, new_ast) ->
          let new_goals =
            List.map new_goals ~f:(Goal.set_prev (Some state.depth))
          in
          let goals = List.append new_goals rest in
          let verif_state = VerifState.with_env verif_state new_env in
          let state' = {state with ast= new_ast; goals; verif_state} in
          Result.Ok state'
  in
  let apply = Rule.mk_apply apply in
  let is_applicable =
    match is_applicable with
    | None -> Rule.mk_default_is_applicable apply
    | Some f ->
        let f' _ state =
          match state.ProverState.goals with
          | [] -> false
          | goal :: _ -> f state.ast goal
        in
        Rule.mk_is_applicable f'
  in
  Rule.{name; is_applicable; is_aggressive; apply}
