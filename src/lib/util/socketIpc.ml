open Core

type handler = In_channel.t -> Out_channel.t -> bool

let try_shutdown inchan outchan fd =
  (* The shutdown function may raise exceptions when the receiving end is offline. Ignore it. *)
  let _ = try In_channel.close inchan with Sys_error _ -> () in
  let _ = try Out_channel.close outchan with Sys_error _ -> () in
  try Unix.shutdown fd ~mode:Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ()

let establish_server server_fun sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
  Unix.bind sock sockaddr ;
  Unix.listen sock 1 ;
  let rec loop () =
    let s, caller = Unix.accept sock in
    Logs.info (fun m ->
        m "Connection accepted from %a" Sexp.pp_hum
          (Unix.sexp_of_sockaddr caller) ) ;
    let inchan = Unix.in_channel_of_descr s
    and outchan = Unix.out_channel_of_descr s in
    let keep_going = server_fun inchan outchan in
    try_shutdown inchan outchan s ;
    Logs.info (fun m ->
        m "Disconnected from %a" Sexp.pp_hum (Unix.sexp_of_sockaddr caller) ) ;
    if keep_going then loop ()
  in
  Logs.debug (fun m ->
      m "Server established at %a" Sexp.pp_hum (Unix.sexp_of_sockaddr sockaddr)
  ) ;
  loop ()

let connect_server client_fun sockaddr =
  let in_chan, out_chan = Unix.open_connection sockaddr in
  Logs.info (fun m ->
      m "Connection established to %a" Sexp.pp_hum
        (Unix.sexp_of_sockaddr sockaddr) ) ;
  let _ = client_fun in_chan out_chan in
  Unix.shutdown_connection in_chan

let establish_remote_server server_fun server_addr port_num =
  establish_server server_fun (Unix.ADDR_INET (server_addr, port_num))

let connect_remote_server client_fun server_addr port_num =
  connect_server client_fun (Unix.ADDR_INET (server_addr, port_num))

let establish_local_server server_fun file =
  establish_server server_fun (Unix.ADDR_UNIX file)

let read_remote_sexp in_chan ~of_sexp =
  try
    let sexp = Sexp.input_sexp in_chan in
    Logs.debug (fun m -> m "REQUEST SEXP: %s" (Sexp.to_string sexp)) ;
    let req = of_sexp sexp in
    Result.Ok req
  with
  | Sexp.Parse_error {err_msg; _} -> Result.Error err_msg
  | End_of_file -> Result.Error "Unexpected EOF in request"
  | Sys_error msg -> Result.Error msg
  | Sexplib.Conv.Of_sexp_error (_, s) ->
      let msg =
        Fmt.strf "sexp conversion failure: %s" (Sexp.to_string_hum s)
      in
      Result.Error msg

let write_remote_sexp out_chan ~sexp_of response =
  let sexp = sexp_of response in
  Logs.debug (fun m -> m "RESPONSE SEXP: %s" (Sexp.to_string_hum sexp)) ;
  Sexp.output_mach out_chan sexp ;
  (* This is not necessary but it's nice to have *)
  Out_channel.newline out_chan ;
  (* This is crucial *)
  Out_channel.flush out_chan
