module IsApplicable : sig
  type t = Ast.Ecoeus.t -> Goal.t -> bool
end

type t =
     Ast.Ecoeus.t
  -> Verifier.PredEnv.t
  -> int
  -> Goal.t
  -> (Goal.t list * Verifier.PredEnv.t * Ast.Ecoeus.t) option

val mk_rule :
     ?is_applicable:IsApplicable.t
  -> ?is_aggressive:bool
  -> name:string
  -> t
  -> Rule.t
