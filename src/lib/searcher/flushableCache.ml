open Core

module FlushConfig = struct
  type t = {file: string option; flush_freq: int}
end

(* The main API of this module *)
module CallWithCache = struct
  type ('k, 'a) t = f:('k -> bool * 'a) -> 'k -> 'a
end

module MakeLocal (C : RuleHistoryCache.S) (V : Core.Sexpable.S) = struct
  module Serializer = RuleHistoryCache.MakeSerializer (C) (V)

  (* type data_t = Serializer.data_t *)

  type cache_t = Serializer.cache_t

  type t = {flush_config: FlushConfig.t; cache: cache_t; acc_size: int ref}

  let create flush_config cache = {flush_config; cache; acc_size= ref 0}

  let call_with_cache {flush_config; cache; acc_size} ~f key =
    match C.get cache ~key with
    | Some data -> data
    | None ->
        let should_cache, data = f key in
        if should_cache then (
          C.set cache ~key ~data ;
          incr acc_size ;
          Option.iter flush_config.file ~f:(fun file ->
              if !acc_size > flush_config.flush_freq then (
                Serializer.store cache file ;
                acc_size := 0 ) ) ) ;
        data
end

module MakeRemote (C : RuleHistoryCache.S) (V : Core.Sexpable.S) = struct
  module Request = RemoteCache.MakeRequest (RuleHistoryCache.Key) (V)
  module Response = RemoteCache.MakeResponse (V)

  type t = {in_chan: In_channel.t; out_chan: Out_channel.t}

  exception RemoteError of string

  let create in_chan out_chan = {in_chan; out_chan}

  let read_response in_chan =
    try
      let sexp = Sexp.input_sexp in_chan in
      Logs.debug (fun m -> m "Cache request sexp: %s" (Sexp.to_string sexp)) ;
      Response.t_of_sexp sexp
    with
    | Sexp.Parse_error {err_msg; _} ->
        let msg = Fmt.strf "Sexp parse error: %s" err_msg in
        raise (RemoteError msg)
    | End_of_file ->
        let msg = Fmt.strf "Unexpected EOF from cache server" in
        raise (RemoteError msg)
    | Sexplib.Conv.Of_sexp_error (_, s) ->
        let msg =
          Fmt.strf "Sexp conversion failure: %s" (Sexp.to_string_hum s)
        in
        raise (RemoteError msg)

  let send_request out_chan req =
    let req_sexp = Request.sexp_of_t req in
    Sexp.output_mach out_chan req_sexp ;
    Out_channel.newline out_chan ;
    Out_channel.flush out_chan

  (* We need to make sure remote_get and remote_set does't crash here *)

  let remote_get {in_chan; out_chan} key =
    try
      send_request out_chan (Request.Get key) ;
      match read_response in_chan with
      | Response.Value v -> v
      | _ as resp ->
          Logs.warn (fun m ->
              m "Unexpected response from cache server: %a" Sexp.pp
                (Response.sexp_of_t resp) ) ;
          None
    with RemoteError msg ->
      Logs.warn (fun m -> m "Remote error: %s" msg) ;
      None

  let remote_set {in_chan; out_chan} key data =
    try
      send_request out_chan (Request.Set (key, data)) ;
      match read_response in_chan with
      | Response.Ack _ -> ()
      | _ as resp ->
          Logs.warn (fun m ->
              m "Unexpected response from cache server: %a" Sexp.pp
                (Response.sexp_of_t resp) ) ;
          ()
    with RemoteError msg ->
      Logs.warn (fun m -> m "Remote error: %s" msg) ;
      ()

  let call_with_cache rc ~f key =
    match remote_get rc key with
    | Some data -> data
    | None ->
        let should_cache, data = f key in
        if should_cache then remote_set rc key data ;
        data
end
