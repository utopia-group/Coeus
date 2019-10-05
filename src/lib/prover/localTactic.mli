module IsApplicable : sig
  type t = Ast.Ecoeus.t -> Goal.t -> bool
end

type t = Ast.Ecoeus.t -> Goal.t -> Rule.t option

val mk_shallow_rule :
     ?is_applicable:IsApplicable.t
  -> ?is_aggressive:bool
  -> name:string
  -> t
  -> Rule.t
