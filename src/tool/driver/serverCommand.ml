open Core

let error_code = 5

module Cache = Searcher.RuleHistoryCache.KeepAll
module CacheValue = Server.ProtocolMessage.Response
module CacheSerializer =
  Searcher.RuleHistoryCache.MakeSerializer (Cache) (CacheValue)

type cache_t = CacheSerializer.cache_t

module LocalCache = Searcher.FlushableCache.MakeLocal (Cache) (CacheValue)
module RemoteCache = Searcher.FlushableCache.MakeRemote (Cache) (CacheValue)

let create_cache opt_file =
  let open Searcher.RuleHistoryCache in
  let init_cache = KeepAll.create () in
  match opt_file with
  | None ->
      let res = (None, init_cache) in
      Result.Ok res
  | Some file -> (
    match Sys.file_exists ~follow_symlinks:true file with
    | `Unknown ->
        let msg =
          Fmt.strf "Cannot figure out whether file \"%s\" exists" file
        in
        Result.Error msg
    | `No ->
        let res = (Some file, init_cache) in
        Result.Ok res
    | `Yes -> (
      try
        CacheSerializer.load file init_cache ;
        let res = (Some file, init_cache) in
        Result.Ok res
      with
      | Sexp.Parse_error {err_msg; _} ->
          let msg = Fmt.strf "Sexp parsing failed: %s" err_msg in
          Result.Error msg
      | Failure msg ->
          let msg = Fmt.strf "Sexp parsing failed: %s" msg in
          Result.Error msg ) )

let connect_remote hostname port =
  try
    let addr =
      match hostname with
      | "localhost" -> Unix.Inet_addr.localhost
      | _ -> Unix.Inet_addr.of_string_or_getbyname hostname
    in
    let sockaddr = Unix.ADDR_INET (addr, port) in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect sock sockaddr ;
    Result.Ok (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
  with Failure msg ->
    let msg = Fmt.strf "Cache server connection error: %s" msg in
    Result.Error msg

let create_cache_memoizer opt_file flush_freq use_remote remote_port =
  let open Result in
  match use_remote with
  | false ->
      create_cache opt_file
      >>= fun (file, cache) ->
      Option.iter file (fun filename ->
          Logs.info (fun m ->
              m "Use \'%s\' as cache backup file. Initial size = %d" filename
                (Cache.length cache) ) ) ;
      let flush_config =
        Searcher.FlushableCache.FlushConfig.{file; flush_freq}
      in
      let local_cache = LocalCache.create flush_config cache in
      let call_with_cache = LocalCache.call_with_cache local_cache in
      Result.Ok call_with_cache
  | true ->
      let remote_hostname = Option.value ~default:"localhost" opt_file in
      connect_remote remote_hostname remote_port
      >>= fun (in_chan, out_chan) ->
      Logs.info (fun m ->
          m "Connected to remote cache server %s:%d" remote_hostname
            remote_port ) ;
      let remote_cache = RemoteCache.create in_chan out_chan in
      let call_with_cache = RemoteCache.call_with_cache remote_cache in
      Result.Ok call_with_cache

let run_remote_server server_config addr port =
  let open Server in
  Logs.info (fun m ->
      m "Coeus remote server starting at %s:%d. "
        (Unix.Inet_addr.to_string addr)
        port ) ;
  try
    let init_state = ServerState.init server_config in
    let handler = ProtocolHandler.handle init_state in
    Util.SocketIpc.establish_remote_server handler addr port ;
    Logs.info (fun m -> m "Coeus remote server succesfully finished")
  with Unix.Unix_error (code, fmsg, _) ->
    let cmsg = Unix.Error.message code in
    Logs.err (fun m -> m "Unix error when calling \"%s\": %s" fmsg cmsg)

let run_server () training_dir testing_dir call_with_cache frontend_config
    search_config addr port =
  let res =
    let open Result in
    call_with_cache
    >>= fun cache_config ->
    Server.Config.create training_dir testing_dir frontend_config cache_config
      search_config
    >>= fun server_config ->
    Logs.info (fun m ->
        m "Training examples are located under \'%s\'"
          (Filename.realpath training_dir) ) ;
    Logs.info (fun m ->
        m "Testing examples are located under \'%s\'"
          (Filename.realpath testing_dir) ) ;
    run_remote_server server_config addr port ;
    Result.Ok ()
  in
  match res with
  | Result.Ok _ -> 0
  | Result.Error msg ->
      Logs.err (fun m -> m "Server initialization error: %s" msg) ;
      error_code

open Cmdliner

let error_info = Term.exit_info ~doc:"server errors" error_code

let training_arg =
  let doc =
    "Specify the training set directory. $(mname) will search for training \
     examples in this directory"
  in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"TRAINING_DIR" ~doc)

let testing_arg =
  let doc =
    "Specify the testing set directory. $(mname) will search for testing \
     examples in this directory"
  in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"TESTING_DIR" ~doc)

let remoteaddr_arg =
  let doc =
    "Specify which remote address the server will bind to. Default to \
     0.0.0.0, which will bind to any remote addresses."
  in
  let env = Arg.env_var "COEUS_REMOTE_ADDR" ~doc in
  let open Arg in
  value & opt (some string) None & info ["a"; "addr"] ~docv:"ADDR" ~doc ~env

let remoteaddr_term =
  Term.(ret (const CommandUtil.get_inet_addr $ remoteaddr_arg))

let remoteport_arg =
  let doc = "Specify the remote port of the server. Default to 12345" in
  let env = Arg.env_var "COEUS_REMOTE_PORT" ~doc in
  Arg.(value & opt int 12345 & info ["p"; "port"] ~docv:"PORT" ~doc ~env)

let use_remote_cache_arg =
  let doc =
    "If this flag is set, $(mname) will use a remote caching server instead \
     of a local cache file, and the --cache argument will be interpreted as a \
     remote server name instead of a file name (default to \"localhost\" if \
     not given)."
  in
  Arg.(value & flag & info ["rc"; "remote-cache"] ~doc)

let remote_cache_port_arg =
  let doc =
    "If --remote-cache flag is set, this argument controls the port number of \
     the remote server. Default to 54321."
  in
  let open Arg in
  value & opt int 54321 & info ["rcp"; "remote-cache-port"] ~doc ~docv:"PORT"

let cache_term =
  let open Term in
  const create_cache_memoizer
  $ CommandUtil.cache_file_arg $ CommandUtil.cache_flush_freq_arg
  $ use_remote_cache_arg $ remote_cache_port_arg

let cmd =
  let doc = "Coeus server" in
  let sdocs = Manpage.s_common_options in
  let man =
    [`S Manpage.s_description; `P "A server connecting to the coeus backend"]
  in
  let exits = error_info :: Term.default_exits in
  ( (let open Term in
    const run_server $ CommandUtil.setup_log_term $ training_arg $ testing_arg
    $ cache_term $ CommandUtil.frontend_config_term
    $ CommandUtil.search_config_term $ remoteaddr_term $ remoteport_arg)
  , Term.info "server" ~doc ~sdocs ~exits ~man )
