open Core
open Verifier
module Ecoeus = Ast.Ecoeus
module IsApplicable = QuasiLocalRule.IsApplicable

type t =
  Ecoeus.t -> PredEnv.t -> int -> Goal.t -> (Goal.t list * PredEnv.t) option

let mk_rule ?is_applicable ?is_aggressive ~name semilocal_rule =
  let quasilocal_rule ast env depth goal =
    Option.map (semilocal_rule ast env depth goal) ~f:(fun (goals, env) ->
        (goals, env, ast) )
  in
  QuasiLocalRule.mk_rule ?is_applicable ?is_aggressive ~name quasilocal_rule
