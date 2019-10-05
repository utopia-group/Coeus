open Core
open Ast.Coeus

exception TransformError of string

let build_decl_map =
  List.fold ~init:Identifier.Map.empty ~f:(fun acc decl ->
      let name = decl.FunDecl.name in
      match Identifier.Map.find acc name with
      | None -> Identifier.Map.set acc ~key:name ~data:decl
      | Some decl' ->
          if not (FunDecl.compare decl decl' = 0) then
            let msg =
              Fmt.strf
                "Found distinct function declarations with the same name: %a"
                Identifier.pp name
            in
            raise (TransformError msg)
          else acc )

let build_proc_map =
  List.fold ~init:Identifier.Map.empty ~f:(fun acc proc ->
      let name = proc.Procedure.name in
      match Identifier.Map.find acc name with
      | None -> Identifier.Map.set acc ~key:name ~data:proc
      | Some _ ->
          let msg =
            Fmt.strf
              "Found distinct procedure definitions with the same name: %a"
              Identifier.pp name
          in
          raise (TransformError msg) )

let process_prog prog =
  let decl_set = Identifier.Hash_set.create () in
  let proc_set = Identifier.Hash_set.create () in
  let rec process_expr e =
    let open Expr in
    match e with
    | LiteralExpr _ | VarExpr _ -> ()
    | UnaryExpr (_, expr) | AnnotatedExpr {expr; _} -> process_expr expr
    | BinaryExpr (_, l, r) -> process_expr l ; process_expr r
    | ArraySelectExpr {base; indices} ->
        process_expr base ;
        List.iter indices ~f:process_expr
    | ArrayStoreExpr {base; indices; value} ->
        process_expr base ;
        List.iter indices ~f:process_expr ;
        process_expr value
    | CondExpr {cond; true_val; false_val} ->
        process_expr cond ; process_expr true_val ; process_expr false_val
    | FunCallExpr {name; args} ->
        Hash_set.add decl_set name ;
        List.iter ~f:process_expr args
    | QuantifiedExpr {body; _} -> process_expr body
  in
  let process_lvalue Lvalue.({indices; _}) =
    List.iter indices ~f:process_expr
  in
  let process_procs procs =
    let rec process_stmts stmts = List.iter ~f:process_stmt stmts
    and process_stmt stmt =
      let open Stmt in
      match stmt with
      | Assume e -> process_expr e
      | Assign {lhs; rhs} -> process_lvalue lhs ; process_expr rhs
      | If {cond; then_branch; else_branch} ->
          process_expr cond ;
          process_stmts then_branch ;
          process_stmts else_branch
      | While {cond; body} -> process_expr cond ; process_stmts body
      | For {lower; upper; step; body; _} ->
          process_expr lower ;
          process_expr upper ;
          process_expr step ;
          process_stmts body
      | Call {rets; name; args} ->
          List.iter rets ~f:process_lvalue ;
          List.iter args ~f:process_expr ;
          Hash_set.add proc_set name
    in
    let process_proc proc = process_stmts proc.Procedure.stmts in
    List.iter procs ~f:process_proc
  in
  let process_spec (spec: Spec.t) =
    List.iter spec.requires ~f:process_expr ;
    List.iter spec.ensures ~f:process_expr
  in
  let process_entryspec (entryspec: EntrySpec.t) =
    Hash_set.add proc_set entryspec.left ;
    Hash_set.add proc_set entryspec.right
  in
  process_procs prog.procs ;
  process_spec prog.spec ;
  process_entryspec prog.entry ;
  (decl_set, proc_set)

let run prog =
  try
    Logs.debug (fun m -> m "DeadDeclEliminator starts") ;
    let decl_map = build_decl_map prog.decls in
    let proc_map = build_proc_map prog.procs in
    let used_decls, used_procs = process_prog prog in
    let decls =
      Identifier.Map.fold decl_map ~init:[] ~f:(fun ~key ~data acc ->
          if Hash_set.mem used_decls key then data :: acc else acc )
    in
    let procs =
      Identifier.Map.fold proc_map ~init:[] ~f:(fun ~key ~data acc ->
          if Hash_set.mem used_procs key then data :: acc else acc )
    in
    let prog = {prog with decls; procs} in
    Logs.debug (fun m -> m "DeadDeclEliminator successfully finished") ;
    Result.Ok prog
  with TransformError msg -> Result.Error msg
