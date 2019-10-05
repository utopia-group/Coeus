open Core
open Ast.Coeus

let run_expr mannot env =
  let replace_var env =
    Expr.map_var ~f:(fun var ->
        match Identifier.Map.find env var with
        | Some var' -> var'
        | None -> var )
  in
  let rec run_expr_impl env e =
    let open Expr in
    match e with
    | LiteralExpr _ | VarExpr _ -> e
    | UnaryExpr (op, e) -> UnaryExpr (op, run_expr_impl env e)
    | BinaryExpr (op, lhs, rhs) ->
        BinaryExpr (op, run_expr_impl env lhs, run_expr_impl env rhs)
    | ArraySelectExpr {base; indices} ->
        ArraySelectExpr
          { base= run_expr_impl env base
          ; indices= List.map ~f:(run_expr_impl env) indices }
    | ArrayStoreExpr {base; indices; value} ->
        ArrayStoreExpr
          { base= run_expr_impl env base
          ; indices= List.map ~f:(run_expr_impl env) indices
          ; value= run_expr_impl env value }
    | AnnotatedExpr {annot; expr} ->
        if Side.equal annot mannot then replace_var env expr else e
    | CondExpr {cond; true_val; false_val} ->
        CondExpr
          { cond= run_expr_impl env cond
          ; true_val= run_expr_impl env true_val
          ; false_val= run_expr_impl env false_val }
    | FunCallExpr {name; args} ->
        FunCallExpr {name; args= List.map args ~f:(run_expr_impl env)}
    | QuantifiedExpr {quantifier; bindings; body} ->
        let env' =
          List.fold bindings ~init:env ~f:(fun acc VarBinding.({name; _}) ->
              Identifier.Map.remove acc name )
        in
        let body = run_expr_impl env' body in
        QuantifiedExpr {quantifier; bindings; body}
  in
  run_expr_impl env

let run_stmts env stmts =
  List.map stmts ~f:(fun stmt ->
      Stmt.map_var stmt ~f:(fun var ->
          match Identifier.Map.find env var with
          | Some var' -> var'
          | None -> var ) )

open FreshIdentifierGenerator

let run_decls env vars pname bindings =
  let run_decl (acc, env) (binding: VarBinding.t) =
    let new_name = rename_fresh_local binding.name vars in
    let new_env =
      Identifier.Map.update env binding.name ~f:(function
        | None -> new_name
        | Some old_name ->
            Logs.warn (fun m ->
                m
                  "Found variable definition with duplicated name in \
                   procedure %a: \"%a\""
                  Identifier.pp pname Identifier.pp old_name ) ;
            new_name )
    in
    ({binding with name= new_name} :: acc, new_env)
  in
  let decls', env' = List.fold bindings ~init:([], env) ~f:run_decl in
  (List.rev decls', env')

let process_spec annot env (spec: Spec.t) =
  let requires = List.map spec.requires ~f:(fun e -> run_expr annot env e) in
  let ensures = List.map spec.ensures ~f:(fun e -> run_expr annot env e) in
  Spec.{requires; ensures}

let run_proc vars spec lentry rentry (proc: Procedure.t) =
  let env = Identifier.Map.empty in
  let params, env = run_decls env vars proc.name proc.params in
  let rets, env = run_decls env vars proc.name proc.rets in
  let locals, env = run_decls env vars proc.name proc.locals in
  let stmts = run_stmts env proc.stmts in
  let spec' =
    if Identifier.equal lentry proc.name then process_spec Side.Left env spec
    else if Identifier.equal rentry proc.name then
      process_spec Side.Right env spec
    else spec
  in
  ({proc with params; rets; locals; stmts}, spec')

let run prog =
  Logs.debug (fun m -> m "VariableRenamer starts") ;
  let vars = VarCollection.create ~size:64 () in
  let entry = prog.entry in
  let procs', spec' =
    List.fold prog.procs ~init:([], prog.spec) ~f:(fun (ps, spec) proc ->
        let proc', spec' = run_proc vars spec entry.left entry.right proc in
        (proc' :: ps, spec') )
  in
  let prog' = {prog with procs= List.rev procs'; spec= spec'} in
  Logs.debug (fun m -> m "VariableRenamer successfully finished") ;
  Result.Ok prog'
