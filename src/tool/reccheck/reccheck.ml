open Core
open Ast.Coeus

let input_error_code = 1

let init_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  ()

module CallGraph = Frontend.CallGraph
module CallGraphSCC = Graph.Components.Make (CallGraph)

let find_recs call_graph =
  let recs_in_scc scc =
    match scc with
    | [] -> None
    | [proc] ->
        if CallGraph.mem_edge call_graph proc proc then Some [proc] else None
    | procs -> Some procs
  in
  let sccs = CallGraphSCC.scc_list call_graph in
  List.iter sccs ~f:(fun scc ->
      match recs_in_scc scc with
      | None -> ()
      | Some procs ->
          Logs.app (fun m ->
              m "Found recursive procedures: %a"
                (Fmt.list ~sep:Fmt.comma Identifier.pp)
                procs ) )

let check_rec file () =
  match Ast.CoeusParselib.parse_file file with
  | Result.Error msg ->
      Logs.err (fun m -> m "%s" msg) ;
      input_error_code
  | Result.Ok prog ->
      Logs.info (fun m -> m "Successfully parsed file %s" file) ;
      let call_graph = CallGraph.Builder.of_coeus prog in
      find_recs call_graph ; 0

open Cmdliner

let input_error_info = Term.exit_info ~doc:"input errors" input_error_code

let setup_log_arg =
  Term.(const init_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let file_arg =
  let doc = "The input Coeus file." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Coeus recursive program detector" in
  let man =
    [ `S Manpage.s_description
    ; `P "Parse a Coeus file and print all recursive calls in it" ]
  in
  let exits = input_error_info :: Term.default_exits in
  ( Term.(const check_rec $ file_arg $ setup_log_arg)
  , Term.info "reccheck" ~version:"v0.1" ~doc ~exits ~man )

let () = Term.(exit_status (eval cmd))
