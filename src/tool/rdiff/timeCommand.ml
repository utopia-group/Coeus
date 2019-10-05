open Core
open Searcher

let pp_file = Fmt.(styled `Underline string)

let get_time status =
  let open SearchResults in
  match status with
  | SearchStatus.Fail ->
      failwith "[INTERNAL] get_time does not support FAIL status"
  | SearchStatus.Timeout -> Float.infinity
  | SearchStatus.Succ (time, _) -> time

let do_diff_single_time thresh file r0 r1 =
  let open SearchResults in
  match (r0, r1) with
  | SearchStatus.Fail, _
   |_, SearchStatus.Fail
   |SearchStatus.Timeout, SearchStatus.Timeout ->
      ()
  | _ ->
      let t0 = get_time r0 in
      let t1 = get_time r1 in
      if Float.abs (t1 -. t0) <= thresh then ()
      else
        let pp_old_time = Fmt.(styled `Cyan float) in
        let pp_new_time =
          if Float.compare t0 t1 > 0 then Fmt.(styled `Green float)
          else Fmt.(styled `Red float)
        in
        Logs.app (fun m ->
            m "@[<hov 2>%a:@ %a@ ->@ %a@]" pp_file file pp_old_time t0
              pp_new_time t1 ) ;
        Logs.app (fun m -> m "")

let do_diff_time thresh domain res0 res1 =
  List.iter domain ~f:(fun file ->
      let r0 = String.Map.find_exn res0 file in
      let r1 = String.Map.find_exn res1 file in
      do_diff_single_time thresh file r0 r1 )

let diff_time file0 file1 thresh () =
  match CommonArgs.load_files file0 file1 with
  | Result.Error msg ->
      Logs.err (fun m -> m "File loading failed: %s" msg) ;
      CommonArgs.load_error_code
  | Result.Ok (res0, res1) ->
      Logs.info (fun m -> m "File loading succeeded") ;
      let domain, res0, res1 = CommonArgs.intersect_domain res0 res1 in
      do_diff_time thresh domain res0 res1 ;
      0

open Cmdliner

let threshold_arg =
  let doc =
    "Specify the threshold of time difference that is considered interesting. \
     Default to 5."
  in
  Arg.(value & opt float 5.0 & info ["t"; "thresh"] ~docv:"THRESHOLD" ~doc)

let threshold_term =
  let check_thresh f =
    if Float.is_negative f then
      let msg = Fmt.strf "Threshold cannot be a negative number: %f" f in
      `Error (false, msg)
    else `Ok f
  in
  Term.(ret (const check_thresh $ threshold_arg))

let cmd =
  let doc = "Time diff" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description; `P "Diff the benchmark solving time"] in
  let exits = CommonArgs.load_error_info :: Term.default_exits in
  ( (let open Term in
    let open CommonArgs in
    const diff_time $ file0_arg $ file1_arg $ threshold_term $ setup_log_term)
  , Term.info "time" ~doc ~sdocs ~man ~exits )
