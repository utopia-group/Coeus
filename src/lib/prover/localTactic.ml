open Core
open Ast.Ecoeus

module IsApplicable = struct
  type t = Ast.Ecoeus.t -> Goal.t -> bool
end

type t = Ast.Ecoeus.t -> Goal.t -> Rule.t option

(* This version of local tactic won't resolve pass the original top rule *)
let mk_shallow_rule ?is_applicable ?(is_aggressive= false) ~name local_tactic =
  let apply config (state: ProverState.t) =
    match state.goals with
    | [] -> Result.Error "Empty active goal"
    | goal :: _ ->
      match local_tactic state.ast goal with
      | None -> Result.Error "Trying to apply a non-applicable tactic"
      | Some rule ->
          let num_goals_limit = ProverState.num_goals state - 1 in
          let rec apply_impl acc tactic_depth (rule: Rule.t) =
            match config.ProverConfig.tactic_depth_limit with
            | Some limit when tactic_depth > limit ->
                let msg = Fmt.strf "Tactic depth limit exceeded" in
                Result.Error msg
            | _ ->
              match rule.apply config acc with
              | Result.Error msg ->
                  let msg = Fmt.strf "Failed tactic application: %s" msg in
                  Result.Error msg
              | Result.Ok acc' ->
                  let acc' = ProverState.resolve_top acc' in
                  let curr_num_goals = ProverState.num_goals acc' in
                  if curr_num_goals <= num_goals_limit then Result.Ok acc'
                  else
                    match acc'.goals with
                    | [] -> Result.Ok acc'
                    | goal :: _ ->
                      match local_tactic acc'.ast goal with
                      | None -> Result.Ok acc'
                      | Some rule ->
                          let tactic_depth = tactic_depth + 1 in
                          apply_impl acc' tactic_depth rule
          in
          let open Result in
          apply_impl state 0 rule
          >>= fun state' ->
          (* We need to adjust the predecessors of the newly created goals *)
          let new_goals, old_goals =
            List.split_n state'.goals
              (num_goals_limit - ProverState.num_goals state')
          in
          match new_goals with
          | [] -> Ok state'
          | _ ->
              let new_goals' =
                List.map new_goals ~f:(Goal.set_prev (Some state.depth))
              in
              let goals = List.append new_goals' old_goals in
              Ok {state' with goals}
  in
  let apply = Rule.mk_apply apply in
  let is_applicable =
    match is_applicable with
    | None -> Rule.mk_default_is_applicable apply
    | Some f ->
        fun _ state ->
          match state.ProverState.goals with
          | [] -> false
          | goal :: _ -> f state.ast goal
  in
  Rule.{name; is_applicable; is_aggressive; apply}
