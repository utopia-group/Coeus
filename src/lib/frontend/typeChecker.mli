val check_var :
     Env.t
  -> Ast.Coeus.Identifier.t option
  -> Ast.Coeus.Identifier.t
  -> (Ast.Ecoeus.VarBinding.t, string) Core.Result.t

val check_expr :
     Env.t
  -> Ast.Coeus.Identifier.t option
  -> Ast.Coeus.Expr.t
  -> (Ast.Ecoeus.Expr.t, string) Core.Result.t

val check_lvalue :
     Env.t
  -> Ast.Coeus.Identifier.t
  -> Ast.Coeus.Lvalue.t
  -> (Ast.Ecoeus.Lvalue.t, string) Core.Result.t

val check_stmt :
     Env.t
  -> Ast.Coeus.Identifier.t
  -> Ast.Coeus.Stmt.t
  -> (Ast.Ecoeus.Stmt.t, string) Core.Result.t

val check_proc :
     Env.t
  -> Ast.Coeus.Procedure.t
  -> (Ast.Ecoeus.Procedure.t, string) Core.Result.t

val check_spec :
  Env.t -> Ast.Coeus.Spec.t -> (Ast.Ecoeus.Spec.t, string) Core.Result.t

val check_prog : Env.t -> Ast.Coeus.t -> (Ast.Ecoeus.t, string) Core.Result.t
