val mk_symmetric_local_rule :
  apply_left:LocalRule.t -> Ast.Ecoeus.Side.t -> LocalRule.t

val mk_symmetric_semilocal_rule :
  apply_left:SemiLocalRule.t -> Ast.Ecoeus.Side.t -> SemiLocalRule.t

val mk_symmetric_quasilocal_rule :
  apply_left:QuasiLocalRule.t -> Ast.Ecoeus.Side.t -> QuasiLocalRule.t

val insert_locals :
     Ast.Ecoeus.t
  -> Ast.Ecoeus.Identifier.t
  -> Ast.Ecoeus.VarBinding.t list
  -> Ast.Ecoeus.t

val create_equivalent_stmts_goal :
     Ast.Ecoeus.t
  -> Ast.Ecoeus.Stmt.t list
  -> Ast.Ecoeus.Stmt.t list
  -> (Goal.t * Ast.Ecoeus.t) option
