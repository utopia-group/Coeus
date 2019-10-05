open Core
open Ast.Ecoeus

let unsat_query_timeout_ms = 1000

let sexps_to_string sexps =
  let buf = Buffer.create 1024 in
  List.iter sexps ~f:(Sexp.to_buffer_mach ~buf) ;
  Buffer.contents buf

let set_solver_timeout env fp timeout_ms =
  let params = Z3.Params.mk_params env.Z3Env.ctx in
  let timeout_key = Z3.Symbol.mk_string env.ctx "timeout" in
  Z3.Params.add_int params timeout_key timeout_ms ;
  Z3.Fixedpoint.set_parameters fp params

let compute_unsat_core env decls vcs query =
  let vc_table = Array.of_list vcs in
  let is_unsat vc_indices =
    let vcs = List.map vc_indices ~f:(Array.get vc_table) in
    let sexps = List.concat [decls; vcs; [query]] in
    let fp = Z3Env.mk_spacer env in
    set_solver_timeout env fp unsat_query_timeout_ms ;
    match Z3.Fixedpoint.parse_string fp (sexps_to_string sexps) with
    | [query] -> (
      try
        match Z3.Fixedpoint.query fp query with
        | Z3.Solver.SATISFIABLE -> true
        | _ -> false
      with Z3.Error _ -> false )
    | _ ->
        let msg =
          "[INTERNAL] UnsatCore parsing can only yield one query expr"
        in
        failwith msg
  in
  let rec do_compute curr other =
    match curr with
    | [] -> []
    | [r] ->
        let cand = r :: other in
        if is_unsat cand then [r] else []
    | _ ->
        let size = List.length curr in
        let half_size = size / 2 in
        let upper_half, lower_half = List.split_n curr half_size in
        let full_upper_half = List.append upper_half other in
        let full_lower_half = List.append lower_half other in
        if is_unsat full_upper_half then do_compute upper_half other
        else if is_unsat full_lower_half then do_compute lower_half other
        else
          let upper_half' = do_compute upper_half full_lower_half in
          let lower_half' =
            do_compute lower_half (List.append upper_half' other)
          in
          List.append upper_half' lower_half'
  in
  let vc_indices = List.init (Array.length vc_table) ~f:(fun i -> i) in
  do_compute vc_indices []
