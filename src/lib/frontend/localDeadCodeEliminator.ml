open Core
open Ast.Coeus

let rec process_stmt stmt =
  let open Stmt in
  match stmt with
  | Assume (Expr.LiteralExpr (Literal.BoolLit true)) -> []
  | Assign {lhs= Lvalue.({base; _}); rhs= Expr.VarExpr v}
    when Identifier.equal base v ->
      []
  | If {cond= Expr.LiteralExpr (Literal.BoolLit b); then_branch; else_branch} ->
      if b then then_branch else else_branch
  | While {cond= Expr.LiteralExpr (Literal.BoolLit false); _} -> []
  | For
      { lower= Expr.LiteralExpr (Literal.IntLit i)
      ; upper= Expr.LiteralExpr (Literal.IntLit j)
      ; direction; _ } ->
      let cmp =
        match direction with
        | Stmt.ForDirection.Forward -> fun x y -> Bigint.(x < y)
        | Stmt.ForDirection.Backward -> fun x y -> Bigint.(x > y)
      in
      if not (cmp i j) then [] else [stmt]
  | _ -> [stmt]

and process_stmts stmts = List.concat_map stmts ~f:process_stmt

let run_on_proc proc =
  let stmts = process_stmts proc.Procedure.stmts in
  {proc with stmts}

let run prog =
  Logs.debug (fun m -> m "LocalDeadCodeEliminator starts") ;
  let prog = proc_map prog ~f:run_on_proc in
  Logs.debug (fun m -> m "LocalDeadCodeEliminator successfully finished") ;
  Result.Ok prog
