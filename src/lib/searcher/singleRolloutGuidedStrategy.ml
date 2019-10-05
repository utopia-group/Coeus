open Core
open Prover

let random_sample rnd_state distr =
  let rnd_num = Random.State.float rnd_state 1.0 in
  let rec impl acc = function
    | [] ->
        let msg = "[INTERNAL] Random sampling should not reach this point" in
        failwith msg
    | (r, s, p) :: rest ->
        if acc <= p then (r, s)
        else
          let acc' = acc -. p in
          impl acc' rest
  in
  impl rnd_num distr

let linear_strategy rand_state remote_guide search_config prover_state =
  match remote_guide search_config prover_state with
  | Result.Error msg ->
      Logs.info (fun m -> m "Remote strategy failed: %s" msg) ;
      None
  | Result.Ok [] -> None
  | Result.Ok distr ->
      let r, s = random_sample rand_state distr in
      Logs.info (fun m -> m "Pick rule %a" Rule.pp r) ;
      Some (r, s)

let single_rollout_strategy rand_state remote_guide =
  LinearStrategy.rollout (linear_strategy rand_state remote_guide)

let repeat_single_rollout_strategy ?count rand_state remote_guide =
  LinearStrategy.repeat_rollout ?count
    (linear_strategy rand_state remote_guide)

let create rand_state =
  RemoteStrategy.to_strategy (single_rollout_strategy rand_state)

let create_repeat ?count rand_state =
  RemoteStrategy.to_strategy (repeat_single_rollout_strategy ?count rand_state)
