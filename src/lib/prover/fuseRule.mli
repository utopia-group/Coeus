open Ast.Ecoeus

val fuse_for_loops :
     Identifier.t
  -> Identifier.t
  -> Identifier.t
  -> Expr.t
  -> Expr.t
  -> Expr.t
  -> Stmt.ForDirection.t
  -> Stmts.t
  -> Stmts.t
  -> Stmts.t
  -> Stmts.t

val create : string -> Side.t -> Rule.t

val fuse_l : Rule.t

val fuse_r : Rule.t
