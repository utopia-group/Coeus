open Core
open Ast.Coeus
module Ecoeus = Ast.Ecoeus

let ltype_of = Ecoeus.Lvalue.type_of

let etype_of = Ecoeus.Expr.type_of

module Context = struct
  type t = Expr of Expr.t | Stmt of Stmt.t

  let pp fmt = function
    | Expr e -> Fmt.pf fmt "  (context: \"%a\")" Expr.pp e
    | Stmt s -> Fmt.pf fmt "  (context: \"%a\")" Stmt.pp s

  let msg = Fmt.strf "%a" pp
end

let check_type ?ctx ~expect actual =
  if not (Type.equal actual expect) then
    let ctx_msg = Option.value_map ~default:"" ~f:Context.msg ctx in
    let msg =
      Fmt.strf
        "Type checking failed: expect \"%a\" but the actual type is \"%a\"%s"
        Type.pp expect Type.pp actual ctx_msg
    in
    Result.Error msg
  else Result.Ok actual

let check_etype ?ctx ~expect e =
  check_type ?ctx ~expect (Ecoeus.Expr.type_of e)

let check_arg_types param_tys arg_tys ~ctx =
  let open Result in
  match List.zip param_tys arg_tys with
  | None ->
      let msg =
        Fmt.strf
          "Type error: mismatched number of formal and actual parameters. \
           Expect %d arguments but found %d (context: \"%a\")"
          (List.length param_tys) (List.length arg_tys) Context.pp ctx
      in
      Error msg
  | Some plist ->
      all
        (List.map plist ~f:(fun (pty, aty) -> check_type ~ctx ~expect:pty aty))
      >>= fun _ -> Ok ()

let check_array_type base_type index_types ~ctx =
  let num_indices = List.length index_types in
  match base_type with
  | Type.ArrayType (idx_types, _) when List.length idx_types = num_indices ->
      let open Result in
      let rec check_array_impl base_type = function
        | [] -> Ok base_type
        | index_type :: index_tys ->
          match base_type with
          | Type.ArrayType (key_ty :: key_tys, val_ty) ->
              check_type index_type ~expect:key_ty ~ctx
              >>= fun _ ->
              let next_base_type =
                if List.is_empty key_tys then val_ty
                else Type.ArrayType (key_tys, val_ty)
              in
              check_array_impl next_base_type index_tys
          | _ ->
              let msg =
                Fmt.strf
                  "Type error: cannot select type \"%a\" from base type \
                   \"%a\" (context: \"%a\")"
                  Type.pp index_type Type.pp base_type Context.pp ctx
              in
              Error msg
      in
      check_array_impl base_type index_types
  | _ when num_indices = 0 -> Result.Ok base_type
  | _ ->
      let msg =
        Fmt.strf "Type error: wrong number of array indices (context: \"%a\")"
          Context.pp ctx
      in
      Result.Error msg

let check_var env opt_parent id =
  match Identifier.Map.find env.Env.var_map id with
  | None ->
      let msg =
        Fmt.strf "Cannot find definition of variable \"%a\"" Identifier.pp id
      in
      Result.Error msg
  | Some Env.VariableInfo.({ty; parent; kind}) ->
      let binding = VarBinding.{name= id; ty} in
      match (opt_parent, parent) with
      | Some curr_proc, Some def_proc
        when not (Identifier.equal curr_proc def_proc) ->
          let msg =
            Fmt.strf
              "Cannot refer to variables \"%a\" defined in procedure %a in \
               procedure %a"
              Identifier.pp id Identifier.pp def_proc Identifier.pp curr_proc
          in
          Result.Error msg
      | None, _ when kind = Env.VariableKind.Local ->
          let msg =
            Fmt.strf "Cannot refer to local variables in specification context"
          in
          Result.Error msg
      | None, Some def_proc -> (
        match Identifier.Map.find env.proc_map def_proc with
        | None ->
            let msg =
              Fmt.strf
                "Cannot find parent of variables in specification context"
            in
            Result.Error msg
        | Some proc_info ->
          match proc_info.Env.ProcInfo.entry_side with
          | None ->
              let msg =
                Fmt.strf
                  "Cannot refer to variables of non-entry procedures in \
                   specification context"
              in
              Result.Error msg
          | _ -> Result.Ok binding )
      | _ -> Result.Ok binding

let rec check_expr env opt_parent expr =
  let open Expr in
  let open Result in
  match expr with
  | LiteralExpr lit -> Ok (Ecoeus.Expr.mk_literal lit)
  | VarExpr v ->
      check_var env opt_parent v
      >>= fun binding -> Ok (Ecoeus.Expr.mk_var binding)
  | UnaryExpr (op, e) ->
      check_expr env opt_parent e
      >>= fun e' ->
      check_etype e' ~ctx:(Context.Expr expr)
        ~expect:(UnaryOperator.op_type_of op)
      >>= fun _ -> Ok (Ecoeus.Expr.mk_unary op e')
  | BinaryExpr (op, lhs, rhs) -> (
      check_expr env opt_parent lhs
      >>= fun lhs' ->
      check_expr env opt_parent rhs
      >>= fun rhs' ->
      match op with
      | BinaryOperator.Eq | BinaryOperator.Ne ->
          (* These two operators are polymorphic *)
          let lhs_ty = etype_of lhs' in
          let rhs_ty = etype_of rhs' in
          check_type ~ctx:(Context.Expr expr) ~expect:lhs_ty rhs_ty
          >>= fun _ -> Ok (Ecoeus.Expr.mk_binary op lhs' rhs')
      | _ ->
          let expect_lhs, expect_rhs = BinaryOperator.op_type_of op in
          check_etype lhs' ~ctx:(Context.Expr expr) ~expect:expect_lhs
          >>= fun _ ->
          check_etype rhs' ~ctx:(Context.Expr expr) ~expect:expect_rhs
          >>= fun _ -> Ok (Ecoeus.Expr.mk_binary op lhs' rhs') )
  | ArraySelectExpr {base; indices} ->
      check_expr env opt_parent base
      >>= fun base' ->
      let base_ty = etype_of base' in
      check_exprs env opt_parent indices
      >>= fun indices' ->
      let index_tys = List.map indices' ~f:etype_of in
      check_array_type base_ty index_tys ~ctx:(Context.Expr expr)
      >>= fun final_ty ->
      Ok (Ecoeus.Expr.mk_array_select base' indices' final_ty)
  | ArrayStoreExpr {base; indices; value} ->
      check_expr env opt_parent base
      >>= fun base' ->
      let base_ty = etype_of base' in
      check_exprs env opt_parent indices
      >>= fun indices' ->
      let index_tys = List.map indices' ~f:etype_of in
      check_array_type base_ty index_tys ~ctx:(Context.Expr expr)
      >>= fun final_ty ->
      check_expr env opt_parent value
      >>= fun value' ->
      check_etype value' ~ctx:(Context.Expr expr) ~expect:final_ty
      >>= fun _ -> Ok (Ecoeus.Expr.mk_array_store base' indices' value')
  | AnnotatedExpr {expr; _} -> check_expr env opt_parent expr
  | CondExpr {cond; true_val; false_val} ->
      check_expr env opt_parent cond
      >>= fun cond' ->
      check_etype cond' ~expect:Type.BoolType ~ctx:(Context.Expr expr)
      >>= fun _ ->
      check_expr env opt_parent true_val
      >>= fun true_val' ->
      check_expr env opt_parent false_val
      >>= fun false_val' ->
      let true_ty = etype_of true_val' in
      check_etype false_val' ~ctx:(Context.Expr expr) ~expect:true_ty
      >>= fun _ -> Ok (Ecoeus.Expr.mk_cond cond' true_val' false_val')
  | FunCallExpr {name; args} -> (
    match Identifier.Map.find env.Env.decl_map name with
    | None ->
        let msg =
          Fmt.strf "Calling an undeclared function (context: %a)" Expr.pp expr
        in
        Error msg
    | Some decl ->
        check_exprs env opt_parent args
        >>= fun args' ->
        let arg_tys = List.map args' ~f:etype_of in
        check_arg_types decl.param_tys arg_tys ~ctx:(Context.Expr expr)
        >>= fun _ -> Ok (Ecoeus.Expr.mk_funcall decl args') )
  | QuantifiedExpr {quantifier; bindings; body} ->
      let extend_binding vmap VarBinding.({name; ty}) =
        let open Env in
        let info =
          VariableInfo.{ty; kind= VariableKind.Quantified; parent= opt_parent}
        in
        Identifier.Map.set vmap ~key:name ~data:info
      in
      let var_map = List.fold bindings ~init:env.var_map ~f:extend_binding in
      let new_env = {env with var_map} in
      check_expr new_env opt_parent body
      >>= fun body' ->
      check_etype body' ~ctx:(Context.Expr expr) ~expect:Type.BoolType
      >>= fun _ -> Ok (Ecoeus.Expr.mk_quantified quantifier bindings body')

and check_exprs env opt_parent =
  let rec check_exprs_impl acc = function
    | [] -> Result.Ok (List.rev acc)
    | e :: rest ->
      match check_expr env opt_parent e with
      | Result.Ok e' ->
          let acc' = e' :: acc in
          check_exprs_impl acc' rest
      | Result.Error msg -> Result.Error msg
  in
  check_exprs_impl []

let check_lvalue env parent Lvalue.({base; indices}) =
  match Identifier.Map.find env.Env.var_map base with
  | None ->
      let msg =
        Fmt.strf "Cannot assign to a nonexisting variable \"%a\"" Identifier.pp
          base
      in
      Result.Error msg
  | Some Env.VariableInfo.({ty; _}) ->
      let open Result in
      all (List.map indices ~f:(check_expr env (Some parent)))
      >>= fun indices' ->
      let indices_ty = List.map indices' ~f:etype_of in
      check_array_type ty indices_ty
        ~ctx:(Context.Expr Lvalue.(to_expr {base; indices}))
      >>= fun final_ty ->
      Ok (Ecoeus.Lvalue.mk_array VarBinding.{name= base; ty} indices' final_ty)

let rec check_stmt_for_counter v stmt =
  let open Stmt in
  let open Result in
  match stmt with
  | Assign {lhs= Lvalue.({base; _}); _} ->
      if Identifier.equal base v then
        let msg =
          Fmt.strf "Cannot assign to counter variable %a in a for loop"
            Identifier.pp v
        in
        Error msg
      else Ok ()
  | If {then_branch; else_branch; _} ->
      check_stmts_for_counter v then_branch
      >>= fun _ -> check_stmts_for_counter v else_branch >>= fun _ -> Ok ()
  | While {body; _} -> check_stmts_for_counter v body >>= fun _ -> Ok ()
  | For {counter; body; _} ->
      if Identifier.equal counter v then
        let msg =
          Fmt.strf
            "Cannot use the same counter variable %a in nested for loops"
            Identifier.pp v
        in
        Error msg
      else check_stmts_for_counter v body >>= fun _ -> Ok ()
  | _ -> Ok ()

and check_stmts_for_counter v = function
  | [] -> Result.Ok ()
  | s :: rest ->
    match check_stmt_for_counter v s with
    | Result.Ok _ -> check_stmts_for_counter v rest
    | _ as e -> e

let rec check_stmt env parent stmt =
  let open Stmt in
  let open Result in
  match stmt with
  | Assume e ->
      check_expr env (Some parent) e
      >>= fun e' ->
      check_etype ~expect:Type.BoolType ~ctx:(Context.Expr e) e'
      >>= fun _ -> Ok (Ecoeus.Stmt.mk_assume parent e')
  | Assign {lhs; rhs} ->
      check_lvalue env parent lhs
      >>= fun lhs' ->
      check_expr env (Some parent) rhs
      >>= fun rhs' ->
      let lhs_ty = ltype_of lhs' in
      check_etype rhs' ~expect:lhs_ty ~ctx:(Context.Expr rhs)
      >>= fun _ -> Ok (Ecoeus.Stmt.mk_assign parent lhs' rhs')
  | If {cond; then_branch; else_branch} ->
      check_expr env (Some parent) cond
      >>= fun cond' ->
      check_etype cond' ~expect:Type.BoolType ~ctx:(Context.Expr cond)
      >>= fun _ ->
      check_stmts env parent then_branch
      >>= fun then_branch' ->
      check_stmts env parent else_branch
      >>= fun else_branch' ->
      Ok (Ecoeus.Stmt.mk_if parent cond' then_branch' else_branch')
  | While {cond; body} ->
      check_expr env (Some parent) cond
      >>= fun cond' ->
      check_etype cond' ~expect:Type.BoolType ~ctx:(Context.Expr cond)
      >>= fun _ ->
      check_stmts env parent body
      >>= fun body' -> Ok (Ecoeus.Stmt.mk_while parent cond' body')
  | For {counter; lower; upper; step; direction; body} ->
      check_var env (Some parent) counter
      >>= fun VarBinding.({name; ty}) ->
      check_expr env (Some parent) lower
      >>= fun lower' ->
      check_expr env (Some parent) upper
      >>= fun upper' ->
      check_expr env (Some parent) step
      >>= fun step' ->
      check_type ty ~expect:Type.IntType
      >>= fun _ ->
      check_etype lower' ~expect:Type.IntType ~ctx:(Context.Expr lower)
      >>= fun _ ->
      check_etype upper' ~expect:Type.IntType ~ctx:(Context.Expr upper)
      >>= fun _ ->
      check_etype step' ~expect:Type.IntType ~ctx:(Context.Expr step)
      >>= fun _ ->
      check_stmts_for_counter name body
      >>= fun _ ->
      check_stmts env parent body
      >>= fun body' ->
      Ok (Ecoeus.Stmt.mk_for parent name lower' upper' step' direction body')
  | Call {rets; name; args} ->
      all (List.map rets ~f:(check_lvalue env parent))
      >>= fun rets' ->
      all (List.map args ~f:(check_expr env (Some parent)))
      >>= fun args' ->
      match Identifier.Map.find env.proc_map name with
      | None ->
          let msg =
            Fmt.strf "Cannot find procedure call target: %a" Identifier.pp name
          in
          Result.Error msg
      | Some proc_info ->
          let arg_tys = List.map args' ~f:etype_of in
          check_arg_types proc_info.proc_sig.param_tys arg_tys
            ~ctx:(Context.Stmt stmt)
          >>= fun _ ->
          let ret_tys = List.map rets' ~f:ltype_of in
          check_arg_types proc_info.proc_sig.ret_tys ret_tys
            ~ctx:(Context.Stmt stmt)
          >>= fun _ -> Ok (Ecoeus.Stmt.mk_call parent rets' name args')

and check_stmts env parent =
  let rec check_stmts_impl acc = function
    | [] -> Result.Ok (List.rev acc)
    | s :: rest ->
      match check_stmt env parent s with
      | Result.Ok s' ->
          let acc' = s' :: acc in
          check_stmts_impl acc' rest
      | Result.Error msg -> Result.Error msg
  in
  check_stmts_impl []

let rec check_proc env Procedure.({name; params; rets; locals; stmts}) =
  let open Result in
  all (List.map stmts ~f:(check_stmt env name))
  >>= fun stmts -> Ok Ecoeus.Procedure.{name; params; rets; locals; stmts}

and check_procs env =
  let rec check_procs_impl acc = function
    | [] -> Result.Ok (List.rev acc)
    | p :: rest ->
      match check_proc env p with
      | Result.Ok p' ->
          let acc' = p' :: acc in
          check_procs_impl acc' rest
      | Result.Error msg -> Result.Error msg
  in
  check_procs_impl []

let check_spec env Spec.({requires; ensures}) =
  let open Result in
  let process_spec_expr e =
    check_expr env None e
    >>= fun e' ->
    check_etype ~ctx:(Context.Expr e) ~expect:Type.BoolType e'
    >>= fun _ -> Ok e'
  in
  all (List.map requires ~f:process_spec_expr)
  >>= fun requires ->
  all (List.map ensures ~f:process_spec_expr)
  >>= fun ensures -> Ok Ecoeus.Spec.{requires; ensures}

let check_prog env prog =
  let open Result in
  check_procs env prog.procs
  >>= fun procs ->
  check_spec env prog.spec
  >>= fun spec ->
  let prog' = Ecoeus.{procs; entry= prog.entry; spec} in
  Result.Ok prog'
