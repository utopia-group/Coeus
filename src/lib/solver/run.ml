open Core
open Verifier
open Ast.Ecoeus

let run_spacer fp query =
  match Z3.Fixedpoint.query fp query with
  | Z3.Solver.UNSATISFIABLE -> "unsat"
  | Z3.Solver.SATISFIABLE -> "sat"
  | Z3.Solver.UNKNOWN -> "unknown"

let sexps_to_string = Fmt.strf "%a" (Fmt.list ~sep:Fmt.nop Sexp.pp_mach)

module IndexList = struct
  type t = int list [@@deriving sexp]
end

let run_unsat_core env decls vcs query =
  let ids = UnsatCore.compute_unsat_core env decls vcs query in
  Sexp.to_string (IndexList.sexp_of_t ids)

let do_verify_spacer (env : Z3Env.t) vstate =
  Logs.debug (fun m -> m "Discharging VCs with spacer...") ;
  let fp = Z3Env.mk_spacer env in
  let vstate = SpacerEncoder.preprocess vstate in
  let decl_sexps, vc_sexps, query_sexp =
    SpacerEncoder.encode_sexp_directly vstate
  in
  let sexps = List.concat [decl_sexps; vc_sexps; [query_sexp]] in
  if env.config.keep_tmp_file then (
    let file = Filename.temp_file "coeus_verif" ".smt2" in
    Sexp.save_sexps_hum file sexps ;
    Logs.info (fun m -> m "Spacer query file has been written to %s" file) ) ;
  match Z3.Fixedpoint.parse_string fp (sexps_to_string sexps) with
  | [query] -> (
      let open Util in
      match
        Subprocess.run_with_timeout env.config.spacer_timeout ~f:(fun () ->
            run_spacer fp query )
      with
      | Result.Error msg ->
          let msg = Fmt.strf "Spacer error: %s" msg in
          Result.Error msg
      | Result.Ok Subprocess.Status.Timeout -> Result.Ok Status.Timeout
      | Result.Ok (Subprocess.Status.Finished s) -> (
        match s with
        | "unsat" -> Result.Ok Status.Verified
        | "unknown" -> Result.Ok Status.Unknown
        | _ -> (
          match env.config.disable_unsat_core with
          | true -> Result.Ok (Status.Rejected [])
          | false ->
              let blames =
                match
                  Subprocess.run_with_timeout env.config.unsat_core_timeout
                    ~f:(fun () ->
                      run_unsat_core env decl_sexps vc_sexps query_sexp )
                with
                | Result.Ok (Subprocess.Status.Finished s) -> (
                  match Sexp.of_string_conv s IndexList.t_of_sexp with
                  | `Error _ -> []
                  | `Result index_list ->
                      (* SpacerEncoder encodes sexps in reverse order *)
                      let rev_vcs = List.rev vstate.VerifState.verif_conds in
                      List.filter_map index_list ~f:(fun vc_idx ->
                          let vc = List.nth_exn rev_vcs vc_idx in
                          vc.blame_step ) )
                | _ -> []
              in
              let blames =
                List.remove_consecutive_duplicates
                  (List.sort blames ~compare:Int.compare)
                  ~equal:Int.equal
              in
              Result.Ok (Status.Rejected blames) ) ) )
  | _ ->
      let msg =
        "[INTERNAL] SpacerEncoder parsing yields can only yield one query expr"
      in
      failwith msg

let run_houdini env ast =
  match Houdini.run_houdini env ast with
  | Status.Verified -> "verified"
  | _ -> "unknown"

let do_verify_houdini (env : Z3Env.t) vstate =
  Logs.debug (fun m -> m "Discharging VCs with houdini...") ;
  let open Util in
  match
    Subprocess.run_with_timeout env.config.houdini_timeout ~f:(fun () ->
        (* The AssignInliner pass in HornEncoder may lead to excessive memory usage. *)
        (* Hack: put HornEncoder in the subprocess so that memory blowup becomes a Houdini failure and therefore do not take down the main process with it. *)
        let horn_ast = HornEncoder.encode vstate in
        run_houdini env horn_ast )
  with
  | Result.Error msg ->
      let msg = Fmt.strf "Spacer error: %s" msg in
      Result.Error msg
  | Result.Ok Subprocess.Status.Timeout -> Result.Ok Status.Timeout
  | Result.Ok (Subprocess.Status.Finished "verified") ->
      Result.Ok Status.Verified
  | _ -> Result.Ok Status.Unknown

let verify (config : SolverConfig.t) vstate =
  if config.disable_houdini && config.disable_spacer then
    let msg = "All solvers has been disabled. Nothing can be proven" in
    Result.Error msg
  else
    let env = Z3Env.create config in
    let houdini_status =
      if config.disable_houdini then
        let _ = Logs.debug (fun m -> m "Houdini is disabled") in
        Result.Ok Status.Unknown
      else do_verify_houdini env vstate
    in
    match houdini_status with
    | Result.Ok Status.Verified ->
        Logs.debug (fun m -> m "Houdini accepted the VCs") ;
        Result.Ok Status.Verified
    | _ ->
        Logs.debug (fun m -> m "Houdini did not accept the VCs") ;
        if config.disable_spacer then
          let _ = Logs.debug (fun m -> m "Spacer is disabled") in
          Result.Ok Status.Unknown
        else do_verify_spacer env vstate
