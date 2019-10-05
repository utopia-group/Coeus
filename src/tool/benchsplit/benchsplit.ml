open Core

let error_code = 1

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

module RunOptions = struct
  type t = {threshold: int; dry_run: bool}
end

let find_benchmark dir =
  match Sys.file_exists ~follow_symlinks:true dir with
  | `No | `Unknown ->
      let msg = Fmt.strf "The specified directory \"%s\" does not exist" dir in
      Result.Error msg
  | `Yes ->
      let open Result in
      let candidates =
        match Sys.is_directory ~follow_symlinks:true dir with
        | `No | `Unknown ->
            let msg =
              Fmt.strf "The specified file \"%s\" is not a directory" dir
            in
            Error msg
        | `Yes ->
            let file_list = Array.to_list (Sys.readdir dir) in
            let file_list =
              List.map file_list ~f:(fun file_name ->
                  Filename.concat dir file_name )
            in
            Ok file_list
      in
      candidates
      >>= fun files ->
      let files =
        files
        |> List.filter ~f:(fun file_path ->
               match Sys.is_directory ~follow_symlinks:true file_path with
               | `No -> Filename.check_suffix file_path ".coeus"
               | _ -> false )
        |> List.map ~f:Filename.realpath
      in
      Ok files

let count_ast_stmts ast =
  let open Ast.Coeus in
  let rec count_stmt stmt =
    let open Stmt in
    match stmt with
    | Assume _ | Assign _ | Call _ -> 1
    | If {then_branch; else_branch; _} ->
        count_stmts then_branch + count_stmts else_branch
    | While {body; _} | For {body; _} -> count_stmts body
  and count_stmts stmts = List.sum (module Int) stmts ~f:count_stmt in
  let count_proc_stmts proc = count_stmts proc.Procedure.stmts in
  List.sum (module Int) ast.procs ~f:count_proc_stmts

let do_split input_dir run_opts =
  let open Result in
  find_benchmark input_dir
  >>= fun benchs ->
  all
    (List.mapi benchs ~f:(fun id file ->
         Ast.CoeusParselib.parse_file file >>= fun ast -> Ok (id, ast) ))
  >>= fun asts ->
  let train_res, test_res =
    List.partition_map asts ~f:(fun (id, ast) ->
        let stmt_size = count_ast_stmts ast in
        if stmt_size <= run_opts.RunOptions.threshold then `Fst id else `Snd id
    )
  in
  let get_benchs = List.map ~f:(List.nth_exn benchs) in
  Ok (get_benchs train_res, get_benchs test_res)

let print_splits train_benchs test_benchs =
  Logs.app (fun m -> m "Training Benchmarks:") ;
  List.iter train_benchs ~f:(fun bench -> Logs.app (fun m -> m "%s" bench)) ;
  Logs.app (fun m -> m "") ;
  Logs.app (fun m -> m "Testing Benchmarks:") ;
  List.iter test_benchs ~f:(fun bench -> Logs.app (fun m -> m "%s" bench))

let perform_splits train_benchs test_benchs train_dir test_dir =
  let do_copy src dst =
    let copy_channels =
      let buf_len = 65536 in
      let buf = Bytes.create buf_len in
      let rec loop ic oc =
        match Pervasives.input ic buf 0 buf_len with
        | 0 -> ()
        | n ->
            Pervasives.output oc buf 0 n ;
            loop ic oc
      in
      loop
    in
    In_channel.with_file ~binary:false src ~f:(fun ic ->
        Out_channel.with_file ~binary:false ~append:false ~fail_if_exists:false
          dst ~f:(copy_channels ic) )
  in
  let copy_bench src ~to_dir =
    let dst = Filename.concat to_dir (Filename.basename src) in
    Logs.debug (fun m -> m "Copying from %s to %s..." src dst) ;
    do_copy src dst
  in
  List.iter train_benchs ~f:(copy_bench ~to_dir:train_dir) ;
  Logs.info (fun m -> m "Training examples copied to %s" train_dir) ;
  List.iter test_benchs ~f:(copy_bench ~to_dir:test_dir) ;
  Logs.info (fun m -> m "Testing examples copied to %s" test_dir)

let split_benchs input_dir opt_train_dir opt_test_dir (run_opts : RunOptions.t)
    () =
  match do_split input_dir run_opts with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      error_code
  | Result.Ok (train_benchs, test_benchs) -> (
      Logs.info (fun m -> m "Splitting completed.") ;
      Logs.info (fun m ->
          m "Training benchmark count = %d. Testing benchmark count = %d"
            (List.length train_benchs) (List.length test_benchs) ) ;
      if run_opts.dry_run then (
        print_splits train_benchs test_benchs ;
        0 )
      else
        match (opt_train_dir, opt_test_dir) with
        | Some train_dir, Some test_dir ->
            perform_splits train_benchs test_benchs train_dir test_dir ;
            0
        | _ ->
            Logs.err (fun m ->
                m
                  "training dir and testing dir are required when --dry-run \
                   is not specified" ) ;
            error_code )

open Cmdliner

let error_info = Term.exit_info ~doc:"on errors" error_code

let input_dir_arg =
  let doc = "A directory containing all Coeus benchmarks." in
  Arg.(required & pos 0 (some dir) None & info [] ~docv:"INPUT_DIR" ~doc)

let train_output_arg =
  let doc = "A directory to hold all training benchmarks." in
  Arg.(value & pos 1 (some dir) None & info [] ~docv:"TRAINING_DIR" ~doc)

let test_output_arg =
  let doc = "A directory to hold all testing benchmarks." in
  Arg.(value & pos 2 (some dir) None & info [] ~docv:"TESTING_DIR" ~doc)

let threshold_arg =
  let doc = "Threshold value for benchmark splitting" in
  Arg.(value & opt int 15 & info ["t"; "threshold"] ~docv:"THRESHOLD" ~doc)

let dry_run_arg =
  let doc = "Perform splitting without actually writing the files" in
  Arg.(value & flag & info ["d"; "dry-run"] ~doc)

let run_opt_term =
  let create_opt threshold dry_run = RunOptions.{threshold; dry_run} in
  Term.(const create_opt $ threshold_arg $ dry_run_arg)

let setup_log_arg =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Benchmark splitter" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Split given benchmarks into a training and a testing set, based on \
         AST sizes" ]
  in
  let exits = error_info :: Term.default_exits in
  ( Term.(
      const split_benchs $ input_dir_arg $ train_output_arg $ test_output_arg
      $ run_opt_term $ setup_log_arg)
  , Term.info "benchsplit" ~version:"v0.1" ~doc ~exits ~man )

let () = Term.(exit_status (eval cmd))
