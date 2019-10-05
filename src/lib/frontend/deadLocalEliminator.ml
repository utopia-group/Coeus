open Core
open Ast.Coeus

let run_on_proc (proc: Procedure.t) =
  let var_set = Stmts.read_write_var_set_of proc.stmts in
  let locals =
    List.filter proc.locals ~f:(fun VarBinding.({name; _}) ->
        Hash_set.mem var_set name )
  in
  {proc with locals}

let run prog =
  Logs.debug (fun m -> m "DeadLocalEliminator starts") ;
  let prog = proc_map prog ~f:run_on_proc in
  Logs.debug (fun m -> m "DeadLocalEliminator successfully finished") ;
  Result.Ok prog
