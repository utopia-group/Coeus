open Core
open Ast.Coeus

let flatten_conjuncts e =
  let rec impl acc = function
    | [] -> acc
    | e :: rest ->
        let open Expr in
        match e with
        | BinaryExpr (BinaryOperator.And, lhs, rhs) ->
            impl acc (lhs :: rhs :: rest)
        | _ -> impl (e :: acc) rest
  in
  impl [] [e]

let collect_assume_conds stmts =
  let rec impl acc = function
    | Stmt.Assume e :: rest ->
        let es = flatten_conjuncts e in
        let acc = List.append es acc in
        impl acc rest
    | _ as rest -> (List.rev acc, rest)
  in
  impl [] stmts

let lift_proc_assumes side (proc: Procedure.t) =
  let conds, stmts = collect_assume_conds proc.stmts in
  let conds =
    List.map conds ~f:(fun cond ->
        let free_vars = Expr.free_vars_of cond in
        let subst_map =
          List.fold free_vars ~init:Identifier.Map.empty ~f:(fun acc key ->
              let data =
                Expr.AnnotatedExpr {annot= side; expr= Expr.VarExpr key}
              in
              Identifier.Map.set acc ~key ~data )
        in
        Expr.subst subst_map cond )
  in
  (conds, {proc with stmts})

let process_prog prog side =
  (* We only deal with assumes at the beginning of the entry procedure *)
  let entry_proc = lookup_entry_proc_exn prog side in
  let lifts, entry_proc = lift_proc_assumes side entry_proc in
  let prog = update_proc prog entry_proc in
  (lifts, prog)

let run prog =
  Logs.debug (fun m -> m "AssumeLifter starts") ;
  let left_lifts, prog = process_prog prog Side.Left in
  let right_lifts, prog =
    if Identifier.equal prog.entry.left prog.entry.right then ([], prog)
    else process_prog prog Side.Right
  in
  let spec =
    let open Spec in
    { prog.spec with
      requires= List.concat [left_lifts; right_lifts; prog.spec.requires] }
  in
  let prog = {prog with spec} in
  Logs.debug (fun m -> m "AssumeLifter successfully finished") ;
  Result.Ok prog
