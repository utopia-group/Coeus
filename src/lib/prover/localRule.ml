open Core
module Ecoeus = Ast.Ecoeus
module IsApplicable = SemiLocalRule.IsApplicable

type t = Ecoeus.t -> int -> Goal.t -> Goal.t option

let mk_rule ?is_applicable ?is_aggressive ~name local_rule =
  let semilocal_rule ast env depth goal =
    Option.map (local_rule ast depth goal) ~f:(fun g -> ([g], env))
  in
  SemiLocalRule.mk_rule ?is_applicable ?is_aggressive ~name semilocal_rule
