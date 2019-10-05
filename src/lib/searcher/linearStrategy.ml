open Core
open Prover

type t = SearchConfig.t -> ProverState.t -> (Rule.t * ProverState.t) option

let rollout (linear_strategy : t) search_config init_state =
  let open Result in
  let rec rollout_impl depth rev_history state =
    match SearchConfig.is_target_state search_config state with
    | true ->
        let rules = List.rev rev_history in
        let search_result =
          let open SearchResult in
          { rules
          ; failed_discharges= 0
          ; num_blames= 0
          ; timeout_discharges= 0
          ; blocked_conflicts= 0 }
        in
        Ok search_result
    | false -> (
        if ProverState.is_fully_resolved state then
          let msg = "Solver disproved the goals or timed out" in
          Error msg
        else
          match linear_strategy search_config state with
          | None ->
              let msg =
                "Failed to resolve all proof goals. The given strategy is not \
                 complete."
              in
              Error msg
          | Some (rule, state') ->
              Logs.debug (fun m -> m "Trying rule %a" Rule.pp rule) ;
              let rev_history = rule :: rev_history in
              let depth = depth + 1 in
              rollout_impl depth rev_history state' )
  in
  rollout_impl 0 [] init_state

let repeat_rollout ?count (strategy : t) search_config init_state =
  match count with
  | Some c when c <= 0 ->
      let msg = Fmt.strf "Repeat rollout count cannot be non-positive: %d" c in
      Result.Error msg
  | _ ->
      let rec repeat_impl num_trials =
        match count with
        | Some c when num_trials >= c ->
            let msg = "Attempt limit reached" in
            Result.Error msg
        | _ -> (
            Logs.info (fun m ->
                let msg =
                  match count with
                  | Some c -> Fmt.strf "Trial %d / %d" (num_trials + 1) c
                  | None -> Fmt.strf "Trial %d" (num_trials + 1)
                in
                m "%s" msg ) ;
            match rollout strategy search_config init_state with
            | Result.Error _ ->
                Logs.info (fun m -> m "Proof attempt failed") ;
                repeat_impl (num_trials + 1)
            | _ as r ->
                Logs.info (fun m -> m "Proof attempt succeeded") ;
                r )
      in
      repeat_impl 0
