let show_help man_format cmds topic () =
  let open Core in
  match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ =
        Cmdliner.Arg.enum (List.rev_map ~f:(fun s -> (s, s)) topics)
      in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" ->
          List.iter ~f:print_endline topics ;
          `Ok 0
      | `Ok t when List.mem cmds t ~equal:String.equal ->
          `Help (man_format, Some t)
      | _ ->
          let msg = Printf.sprintf "Unhandled topic: %s" topic in
          `Error (false, msg)

open Cmdliner

let help_secs =
  [ `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S Manpage.s_bugs
  ; `P "Check bug reports at https://github.com/grievejia/Coeus/issues" ]

let cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about coeus and coeus commands" in
  let man =
    [ `S Manpage.s_description
    ; `P "Prints help about coeus commands and other subjects..."
    ; `Blocks help_secs ]
  in
  ( (let open Term in
    ret
      ( const show_help $ Arg.man_format $ choice_names $ topic
      $ CommandUtil.setup_log_term ))
  , Term.info "help" ~doc ~exits:Term.default_exits ~man )
