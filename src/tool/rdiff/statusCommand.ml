open Core
open Searcher

let pp_file = Fmt.(styled `Underline string)

let do_diff_single_stat file r0 r1 =
  let open SearchResults in
  match (r0, r1) with
  | SearchStatus.Fail, SearchStatus.Fail
   |SearchStatus.Timeout, SearchStatus.Timeout
   |SearchStatus.Succ _, SearchStatus.Succ _ ->
      ()
  | _ ->
      let pp_old_status = Fmt.(styled `Cyan SearchStatus.pp) in
      let pp_new_status =
        if [%compare : SearchStatus.t] r0 r1 < 0 then
          Fmt.(styled `Green SearchStatus.pp)
        else Fmt.(styled `Red SearchStatus.pp)
      in
      Logs.app (fun m ->
          m "@[<hov 2>%a:@ %a@ ->@ %a@]" pp_file file pp_old_status r0
            pp_new_status r1 ) ;
      Logs.app (fun m -> m "")

let do_diff_stat domain res0 res1 =
  List.iter domain ~f:(fun file ->
      let r0 = String.Map.find_exn res0 file in
      let r1 = String.Map.find_exn res1 file in
      do_diff_single_stat file r0 r1 )

let diff_stat file0 file1 () =
  match CommonArgs.load_files file0 file1 with
  | Result.Error msg ->
      Logs.err (fun m -> m "File loading failed: %s" msg) ;
      CommonArgs.load_error_code
  | Result.Ok (res0, res1) ->
      Logs.info (fun m -> m "File loading succeeded") ;
      let domain, res0, res1 = CommonArgs.intersect_domain res0 res1 in
      do_diff_stat domain res0 res1 ;
      0

open Cmdliner

let cmd =
  let doc = "Status diff" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P "Diff the solving status (succeeded/failed/timeout)" ]
  in
  let exits = CommonArgs.load_error_info :: Term.default_exits in
  ( (let open Term in
    CommonArgs.(const diff_stat $ file0_arg $ file1_arg $ setup_log_term))
  , Term.info "status" ~doc ~sdocs ~man ~exits )
