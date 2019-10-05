open Core
open Searcher

type call_with_cache_t =
  ( RuleHistoryCache.Key.t
  , ProtocolMessage.Response.t )
  FlushableCache.CallWithCache.t

type t =
  { training_benchs: string array
  ; testing_benchs: string array
  ; frontend_config: Frontend.FrontendConfig.t
  ; call_with_cache: call_with_cache_t
  ; search_config: SearchConfig.t }

let bench_of_id {training_benchs; testing_benchs; _} id =
  let training_num = Array.length training_benchs in
  if id < 0 then None
  else if id < training_num then Some training_benchs.(id)
  else
    let id = id - training_num in
    let testing_num = Array.length testing_benchs in
    if id < testing_num then Some testing_benchs.(id) else None

let get_bench_files dir =
  match Sys.is_directory ~follow_symlinks:true dir with
  | `Yes ->
      let bench_files = Sys.readdir dir in
      let bench_files =
        Array.filter bench_files ~f:(String.is_suffix ~suffix:".coeus")
      in
      Array.sort bench_files ~compare:String.compare ;
      let benchs =
        Array.map bench_files ~f:(fun bench_file ->
            Fmt.strf "%s/%s" dir bench_file )
      in
      Result.Ok benchs
  | _ ->
      let msg = Fmt.strf "Not a directory: %s" dir in
      Result.Error msg

let create training_dir testing_dir frontend_config call_with_cache
    search_config =
  let open Result in
  get_bench_files training_dir
  >>= fun training_benchs ->
  get_bench_files testing_dir
  >>= fun testing_benchs ->
  Result.Ok
    { training_benchs
    ; testing_benchs
    ; frontend_config
    ; call_with_cache
    ; search_config }
