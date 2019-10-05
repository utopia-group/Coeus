module IsApplicable = Rule.IsApplicable

type t = ProverConfig.t -> ProverState.t -> Rule.t option

val mk_rule :
     ?is_applicable:IsApplicable.t
  -> ?is_aggressive:bool
  -> name:string
  -> t
  -> Rule.t
