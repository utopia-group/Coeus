open Core

type t = {ctx: Z3.context; config: SolverConfig.t}

let create config =
  let open Z3 in
  let ctx = mk_context [("model", "true"); ("proof", "false")] in
  {ctx; config}

let add_timeout_param ctx timeout params =
  let timeout_key = Z3.Symbol.mk_string ctx "timeout" in
  let timeout_val = Float.to_int (Time.Span.to_ms timeout) in
  Z3.Params.add_int params timeout_key timeout_val

let mk_solver {ctx; config} =
  let open Z3 in
  let solver = Z3.Solver.mk_solver ctx None in
  let params = Params.mk_params ctx in
  add_timeout_param ctx config.houdini_timeout params ;
  Z3.Solver.set_parameters solver params ;
  solver

let add_engine_param {ctx; _} params =
  let engine_key = Z3.Symbol.mk_string ctx "engine" in
  let engine_value = Z3.Symbol.mk_string ctx "spacer" in
  Z3.Params.add_symbol params engine_key engine_value

let mk_spacer ({ctx; _} as env) =
  let open Z3 in
  let fp = Fixedpoint.mk_fixedpoint ctx in
  let params = Params.mk_params ctx in
  add_engine_param env params ;
  Fixedpoint.set_parameters fp params ;
  fp
