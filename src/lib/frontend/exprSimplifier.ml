open Core
open Ast.Coeus

let run_on_proc proc =
  let stmts = Stmts.map_expr ~f:Expr.simplify proc.Procedure.stmts in
  {proc with stmts}

let run prog =
  Logs.debug (fun m -> m "ExprSimplifier starts") ;
  let prog = proc_map prog ~f:run_on_proc in
  Logs.debug (fun m -> m "ExprSimplifer successfully finished") ;
  Result.Ok prog
