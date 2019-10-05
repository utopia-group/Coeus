open Core

let load_error_code = 1

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

let load_files file0 file1 =
  let open Result in
  let open Searcher in
  SearchResults.load file0
  >>= fun res0 -> SearchResults.load file1 >>= fun res1 -> Ok (res0, res1)

let intersect_domain res0 res1 =
  let open Searcher in
  let create_key_set res = String.Hash_set.of_list (String.Map.keys res) in
  let domain0 = create_key_set res0 in
  let domain1 = create_key_set res1 in
  if not (Hash_set.equal domain0 domain1) then
    Logs.warn (fun m ->
        m "The two evaluations operate on different benchmark set" ) ;
  let domain = Hash_set.inter domain0 domain1 in
  Logs.info (fun m ->
      m "Intersected domain contains %d items" (Hash_set.length domain) ) ;
  let filter_res res = String.Map.filter_keys res ~f:(Hash_set.mem domain) in
  (Hash_set.to_list domain, filter_res res0, filter_res res1)

open Cmdliner

let load_error_info = Term.exit_info ~doc:"file load errors" load_error_code

let file0_arg =
  let doc = "Base file to diff" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"BASE_FILE" ~doc)

let file1_arg =
  let doc = "Alternative file to diff" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"ALT_FILE" ~doc)

let setup_log_term =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
