open Core
open Prover

type t =
     (   SearchConfig.t
      -> ProverState.t
      -> ((Rule.t * ProverState.t * float) list, string) Result.t)
  -> Strategy.t

module Request = struct
  open Util.Feature

  type t = NextState of StateFeature.t * ActionList.t
  [@@deriving sexp, compare]
end

module Response = struct
  open Util.Feature

  type t = Probability of ProbDistribution.t [@@deriving sexp, compare]
end

let get_distribution in_chan out_chan search_config prover_state =
  let prover_config = search_config.SearchConfig.prover_config in
  let next_rule_index_states =
    ProverAction.applicable_candidate_rule_indices_with_states prover_config
      prover_state
  in
  match next_rule_index_states with
  | [] ->
      let msg = Fmt.strf "Cannot find any available rules" in
      Result.Error msg
  | _ ->
      let next_rule_indices =
        List.map next_rule_index_states (fun (i, _) -> i)
      in
      let state_features = StateEncoder.encode prover_config prover_state in
      let action_features = ActionEncoder.encode_actions next_rule_indices in
      let req = Request.NextState (state_features, action_features) in
      Util.SocketIpc.write_remote_sexp ~sexp_of:Request.sexp_of_t out_chan req ;
      let open Result in
      Util.SocketIpc.read_remote_sexp ~of_sexp:Response.t_of_sexp in_chan
      >>= fun resp ->
      match resp with Response.Probability probs ->
        match List.zip next_rule_index_states (Array.to_list probs) with
        | None ->
            let msg =
              Fmt.strf
                "Probability count mismatch: there are %d available actions \
                 while received %d probabilities"
                (List.length next_rule_indices)
                (Array.length probs)
            in
            Result.Error msg
        | Some l ->
            let res =
              List.map l ~f:(fun ((i, s), p) ->
                  let r = Rules.candidate_rules.(i) in
                  (r, s, p) )
            in
            Result.Ok res

let handler res (remote_strategy: t) search_config init_state in_chan out_chan =
  let guide = get_distribution in_chan out_chan in
  res := remote_strategy guide search_config init_state ;
  false

let to_strategy (remote_strategy: t) (addr, port) search_config init_state =
  Logs.info (fun m ->
      m "Coeus remote server connecting to %s:%d. "
        (Unix.Inet_addr.to_string addr)
        port ) ;
  try
    (* The server API does not return any meaningful value so we need to pass info via mutable var *)
    let res = ref (Result.Error "Unknown remote error") in
    Util.SocketIpc.connect_remote_server
      (handler res remote_strategy search_config init_state)
      addr port ;
    !res
  with Unix.Unix_error (code, fmsg, _) ->
    let cmsg = Unix.Error.message code in
    let msg = Fmt.strf "Unix error when calling \"%s\": %s" fmsg cmsg in
    Result.Error msg
