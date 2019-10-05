open Core
open Ast.Coeus

exception TransformError of string

let add_env env VarBinding.({name; ty}) =
  Identifier.Map.set env ~key:name ~data:ty

let create_decl_env prog =
  List.fold prog.decls ~init:Identifier.Map.empty ~f:
    (fun acc (decl: FunDecl.t) -> Identifier.Map.set acc decl.name decl )

let create_proc_env prog =
  List.fold prog.procs ~init:Identifier.Map.empty ~f:
    (fun acc (proc: Procedure.t) ->
      let proc_sig = ProcSignature.of_proc proc in
      Identifier.Map.set acc proc.name proc_sig )

let run_literal ty lit =
  let open Type in
  let open Literal in
  match (ty, lit) with
  | BoolType, IntLit i ->
      if Bigint.equal i Bigint.zero then BoolLit false else BoolLit true
  | IntType, BoolLit false -> IntLit Bigint.zero
  | IntType, BoolLit true -> IntLit Bigint.one
  | _, _ -> lit

let bool_to_int cond =
  Expr.CondExpr
    { cond
    ; true_val= Expr.LiteralExpr (Literal.IntLit Bigint.one)
    ; false_val= Expr.LiteralExpr (Literal.IntLit Bigint.zero) }

let int_to_bool e =
  Expr.BinaryExpr
    (BinaryOperator.Ne, e, Expr.LiteralExpr (Literal.IntLit Bigint.zero))

let fix_type_by_cast decl_env env ty e =
  let ret_env =
    Identifier.Map.map decl_env ~f:(fun decl -> decl.FunDecl.ret_ty)
  in
  match Expr.quick_type_of ~ret_env ~var_env:env e with
  | None -> (* Nothing we can do here *)
            e
  | Some ty' ->
      let open Type in
      match (ty, ty') with
      | t0, t1 when Type.equal t0 t1 -> e
      | BoolType, IntType -> int_to_bool e
      | IntType, BoolType -> bool_to_int e
      | _, _ ->
          Logs.warn (fun m ->
              m "Cannot fix type to %a by cast: %a" Type.pp ty Expr.pp e ) ;
          e

let rec run_expr decl_env env ty e =
  let open Expr in
  match e with
  | LiteralExpr lit -> LiteralExpr (run_literal ty lit)
  | VarExpr _ -> fix_type_by_cast decl_env env ty e
  | UnaryExpr (UnaryOperator.Not, Expr.LiteralExpr (Literal.IntLit i))
    when ty = Type.BoolType && i = Bigint.zero ->
      Expr.LiteralExpr (Literal.BoolLit true)
  | UnaryExpr (UnaryOperator.Not, Expr.LiteralExpr (Literal.IntLit i))
    when ty = Type.BoolType && i = Bigint.one ->
      Expr.LiteralExpr (Literal.BoolLit false)
  | UnaryExpr (op, e) -> run_unary decl_env env ty op e
  | BinaryExpr (op, lhs, rhs) -> run_binary decl_env env ty op lhs rhs
  | ArraySelectExpr {base; indices} ->
      (* Currently we only allow indices of int types *)
      let indices = List.map indices ~f:(run_expr decl_env env Type.IntType) in
      let e' = ArraySelectExpr {base; indices} in
      fix_type_by_cast decl_env env ty e'
  | ArrayStoreExpr {base; indices; value} ->
      (* Currently we only allow indices of int types *)
      let indices = List.map indices ~f:(run_expr decl_env env Type.IntType) in
      let value = run_expr decl_env env ty value in
      ArrayStoreExpr {base; indices; value}
  | AnnotatedExpr {annot; expr} ->
      AnnotatedExpr {annot; expr= run_expr decl_env env ty expr}
  | CondExpr {cond; true_val; false_val} ->
      let cond = run_expr decl_env env Type.BoolType cond in
      let true_val = run_expr decl_env env ty true_val in
      let false_val = run_expr decl_env env ty false_val in
      CondExpr {cond; true_val; false_val}
  | FunCallExpr {name; args} -> (
    match Identifier.Map.find decl_env name with
    | None -> e
    | Some (decl: FunDecl.t) ->
      match List.zip decl.param_tys args with
      | None -> e
      | Some ty_exprs ->
          let args =
            List.map ty_exprs ~f:(fun (ty, expr) ->
                run_expr decl_env env ty expr )
          in
          let e = FunCallExpr {name; args} in
          fix_type_by_cast decl_env env ty e )
  | QuantifiedExpr {bindings; body; _} ->
      let env = List.fold bindings ~init:env ~f:add_env in
      match ty with
      | Type.BoolType -> run_expr decl_env env Type.BoolType body
      | _ ->
          let msg = "TypeFixer found quantified expr with non-bool type" in
          raise (TransformError msg)

and run_unary decl_env env ty op e =
  let open UnaryOperator in
  let giveup () =
    let msg =
      Fmt.strf
        "TypeFixer detected a non-recoverable type error: %a (required type = \
         %a)"
        Expr.pp
        (Expr.UnaryExpr (op, e))
        Type.pp ty
    in
    raise (TransformError msg)
  in
  match op with
  | Neg -> (
    match ty with
    | Type.IntType ->
        let e' = run_expr decl_env env ty e in
        Expr.UnaryExpr (op, e')
    | Type.BoolType ->
        let e' = run_expr decl_env env ty e in
        bool_to_int e'
    | _ -> giveup () )
  | Not ->
    match ty with
    | Type.BoolType ->
        let e' = run_expr decl_env env ty e in
        Expr.UnaryExpr (op, e')
    | Type.IntType ->
        let e' = run_expr decl_env env ty e in
        int_to_bool e'
    | _ -> giveup ()

and run_binary decl_env env ty op lhs rhs =
  let open BinaryOperator in
  let giveup () =
    let msg =
      Fmt.strf
        "TypeFixer detected a non-recoverable type error: %a (required type = \
         %a)"
        Expr.pp
        (Expr.BinaryExpr (op, lhs, rhs))
        Type.pp ty
    in
    raise (TransformError msg)
  in
  let fix_int_op () =
    match ty with
    | Type.IntType ->
        let lhs' = run_expr decl_env env ty lhs in
        let rhs' = run_expr decl_env env ty rhs in
        Expr.BinaryExpr (op, lhs', rhs')
    | Type.BoolType ->
        let lhs' = run_expr decl_env env ty lhs in
        let rhs' = run_expr decl_env env ty rhs in
        let e = Expr.BinaryExpr (op, lhs', rhs') in
        int_to_bool e
    | _ -> giveup ()
  in
  let fix_bool_op () =
    let lhs' = run_expr decl_env env Type.BoolType lhs in
    let rhs' = run_expr decl_env env Type.BoolType rhs in
    let e' = Expr.BinaryExpr (op, lhs', rhs') in
    fix_type_by_cast decl_env env ty e'
  in
  let fix_int_rel_op () =
    let ret_env =
      Identifier.Map.map decl_env ~f:(fun decl -> decl.FunDecl.ret_ty)
    in
    match Expr.quick_type_of ~ret_env ~var_env:env lhs with
    | Some Type.IntType ->
        let lhs' = run_expr decl_env env Type.IntType lhs in
        let rhs' = run_expr decl_env env Type.IntType rhs in
        let e = Expr.BinaryExpr (op, lhs', rhs') in
        fix_type_by_cast decl_env env ty e
    | _ -> giveup ()
  in
  match op with
  | Plus -> fix_int_op ()
  | Minus -> fix_int_op ()
  | Mult -> fix_int_op ()
  | Div -> fix_int_op ()
  | Mod -> fix_int_op ()
  | And | Or | Imply -> fix_bool_op ()
  | Lt -> fix_int_rel_op ()
  | Le -> fix_int_rel_op ()
  | Gt -> fix_int_rel_op ()
  | Ge -> fix_int_rel_op ()
  | Eq | Ne ->
      let ret_env =
        Identifier.Map.map decl_env ~f:(fun decl -> decl.FunDecl.ret_ty)
      in
      match Expr.quick_type_of ~ret_env ~var_env:env lhs with
      | Some lhs_ty -> (
          let lhs' = run_expr decl_env env lhs_ty lhs in
          let rhs' = run_expr decl_env env lhs_ty rhs in
          let e' = Expr.BinaryExpr (op, lhs', rhs') in
          match ty with
          | Type.BoolType -> e'
          | Type.IntType -> bool_to_int e'
          | _ -> giveup () )
      | None -> (* Nothing we can do here *)
                Expr.BinaryExpr (op, lhs, rhs)

let rec run_stmt decl_env proc_env env stmt =
  let open Stmt in
  match stmt with
  | Assume _ ->
      (* Currently there's no need to fix assume stmts since they don't appear in real-world source codes *)
      stmt
  | Assign {lhs; rhs} -> (
    match Lvalue.quick_type_of env lhs with
    | Some lty -> Assign {lhs; rhs= run_expr decl_env env lty rhs}
    | None -> stmt )
  | If {cond; then_branch; else_branch} ->
      let cond = run_expr decl_env env Type.BoolType cond in
      let then_branch =
        List.map then_branch ~f:(run_stmt decl_env proc_env env)
      in
      let else_branch =
        List.map else_branch ~f:(run_stmt decl_env proc_env env)
      in
      If {cond; then_branch; else_branch}
  | While {cond; body} ->
      let cond = run_expr decl_env env Type.BoolType cond in
      let body = List.map body ~f:(run_stmt decl_env proc_env env) in
      While {cond; body}
  | For {counter; lower; upper; step; direction; body} ->
      let lower = run_expr decl_env env Type.IntType lower in
      let upper = run_expr decl_env env Type.IntType upper in
      let step = run_expr decl_env env Type.IntType step in
      let body = List.map body ~f:(run_stmt decl_env proc_env env) in
      For {counter; lower; upper; step; direction; body}
  | Call {rets; name; args} ->
    match Identifier.Map.find proc_env name with
    | None -> stmt
    | Some proc_sig ->
      match List.zip args proc_sig.ProcSignature.param_tys with
      | None -> stmt
      | Some pairs ->
          let args =
            List.map pairs ~f:(fun (arg, ty) -> run_expr decl_env env ty arg)
          in
          Call {rets; name; args}

let run_proc decl_env proc_env (proc: Procedure.t) =
  Logs.info (fun m ->
      m "TypeFixer runs at procedure %a" Identifier.pp proc.name ) ;
  let env = List.fold proc.params ~init:Identifier.Map.empty ~f:add_env in
  let env = List.fold proc.rets ~init:env ~f:add_env in
  let env = List.fold proc.locals ~init:env ~f:add_env in
  let stmts = List.map ~f:(run_stmt decl_env proc_env env) proc.stmts in
  Logs.info (fun m ->
      m "TypeFixer successfully finished at procedure %a" Identifier.pp
        proc.name ) ;
  {proc with stmts}

let run prog =
  try
    let decl_env = create_decl_env prog in
    let proc_env = create_proc_env prog in
    let prog' = proc_map prog ~f:(run_proc decl_env proc_env) in
    Result.Ok prog'
  with TransformError msg -> Result.Error msg
