module IsApplicable = SemiLocalRule.IsApplicable

type t = Ast.Ecoeus.t -> int -> Goal.t -> Goal.t option

val mk_rule :
     ?is_applicable:IsApplicable.t
  -> ?is_aggressive:bool
  -> name:string
  -> t
  -> Rule.t
