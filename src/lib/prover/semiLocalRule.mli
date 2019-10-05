module IsApplicable = QuasiLocalRule.IsApplicable

type t =
     Ast.Ecoeus.t
  -> Verifier.PredEnv.t
  -> int
  -> Goal.t
  -> (Goal.t list * Verifier.PredEnv.t) option

val mk_rule :
     ?is_applicable:IsApplicable.t
  -> ?is_aggressive:bool
  -> name:string
  -> t
  -> Rule.t
