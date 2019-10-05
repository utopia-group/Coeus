open Core
open Ast.Coeus

let mult_name = Identifier.of_string "intmult"

let div_name = Identifier.of_string "intdiv"

let mod_name = Identifier.of_string "intmod"

let mult_decl () =
  let open FunDecl in
  { name= mult_name
  ; param_tys= [Type.IntType; Type.IntType]
  ; ret_ty= Type.IntType }

let div_decl () =
  let open FunDecl in
  { name= div_name
  ; param_tys= [Type.IntType; Type.IntType]
  ; ret_ty= Type.IntType }

let mod_decl () =
  let open FunDecl in
  { name= mod_name
  ; param_tys= [Type.IntType; Type.IntType]
  ; ret_ty= Type.IntType }

let get_decls () = [mult_decl (); div_decl (); mod_decl ()]

let rec process_expr e =
  let open Expr in
  match e with
  | LiteralExpr _ | VarExpr _ -> e
  | UnaryExpr (op, e) -> UnaryExpr (op, process_expr e)
  | BinaryExpr (op, lhs, rhs) -> (
      let open BinaryOperator in
      let lhs = process_expr lhs in
      let rhs = process_expr rhs in
      match op with
      | Div -> FunCallExpr {name= div_name; args= [lhs; rhs]}
      | Mod -> FunCallExpr {name= mod_name; args= [lhs; rhs]}
      | Mult -> FunCallExpr {name= mult_name; args= [lhs; rhs]}
      | _ -> BinaryExpr (op, lhs, rhs) )
  | ArraySelectExpr {base; indices} ->
      ArraySelectExpr
        {base= process_expr base; indices= List.map indices ~f:process_expr}
  | ArrayStoreExpr {base; indices; value} ->
      ArrayStoreExpr
        { base= process_expr base
        ; indices= List.map indices ~f:process_expr
        ; value= process_expr value }
  | AnnotatedExpr {annot; expr} ->
      AnnotatedExpr {annot; expr= process_expr expr}
  | CondExpr {cond; true_val; false_val} ->
      CondExpr
        { cond= process_expr cond
        ; true_val= process_expr true_val
        ; false_val= process_expr false_val }
  | FunCallExpr {name; args} ->
      FunCallExpr {name; args= List.map args ~f:process_expr}
  | QuantifiedExpr {quantifier; bindings; body} ->
      QuantifiedExpr {quantifier; bindings; body= process_expr body}

let process_stmts = Stmts.map_expr ~f:process_expr

let run_on_proc proc =
  let stmts = process_stmts proc.Procedure.stmts in
  {proc with stmts}

let run_on_prog prog =
  let procs = List.map ~f:run_on_proc prog.procs in
  let decls = List.append (get_decls ()) prog.decls in
  {prog with procs; decls}

let run prog =
  Logs.debug (fun m -> m "NonlinearArithEliminator starts") ;
  let prog' = run_on_prog prog in
  Logs.debug (fun m -> m "NonlinearArithEliminator successfully finished") ;
  Result.Ok prog'
