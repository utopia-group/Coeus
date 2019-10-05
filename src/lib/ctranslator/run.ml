open Core
open Cparser
open Ast.Coeus

module IdentiferGenerator = struct
  type t = {counter: int ref}

  let create () = {counter= ref 0}

  let get ?(name= "tmp") {counter} =
    let name = Fmt.strf ".%s_%d" name !counter in
    incr counter ; Identifier.of_string name
end

let get_func_identifier side i =
  let iint = Camlcoq.P.to_int i in
  let descr = match side with Side.Left -> "LEFT" | Side.Right -> "RIGHT" in
  let name = Fmt.strf "%s_func%d" descr iint in
  Identifier.of_string name

module BitvecBuiltin = struct
  type t = BvAnd | BvOr | BvXor | BvNot | BvShl | BvShr

  let all = [BvAnd; BvOr; BvXor; BvNot; BvShl; BvShr]

  let name_of = function
    | BvAnd -> "bvand"
    | BvOr -> "bvor"
    | BvXor -> "bvxor"
    | BvNot -> "bvnot"
    | BvShl -> "bvshl"
    | BvShr -> "bvshr"

  let ident_of b = Identifier.of_string (name_of b)

  let param_types_of b =
    let open Type in
    match b with
    | BvAnd | BvOr | BvXor | BvShl | BvShr -> [IntType; IntType]
    | BvNot -> [IntType]

  let ret_type_of _ = Type.IntType

  let decl_of b =
    let open FunDecl in
    {name= ident_of b; param_tys= param_types_of b; ret_ty= ret_type_of b}
end

module BuiltinExternal = struct
  type t = Assume
end

module Env = struct
  type t =
    { var_map: Identifier.t option Maps.PMap.t
    ; decl_map: BuiltinExternal.t option Maps.PMap.t
    ; id_gen: IdentiferGenerator.t
    ; side: Side.t
    ; ret_name: Identifier.t }
end

let extract_fun_defs side name (c_prog: Csyntax.program) =
  let defs, decls =
    List.fold ~init:([], []) c_prog.prog_defs ~f:
      (fun (def_acc, decl_acc) (ident, gdef) ->
        match gdef with
        | AST.Gfun (Ctypes.Internal fun_def) ->
            let funid = get_func_identifier side ident in
            ((funid, fun_def) :: def_acc, decl_acc)
        | AST.Gfun (Ctypes.External (AST.EF_external (name_chars, _), _, _, _))
          when String.(equal "assume" (of_char_list name_chars)) ->
            (* Special-handling of built-in assumptions *)
            (def_acc, (ident, BuiltinExternal.Assume) :: decl_acc)
        | _ -> (def_acc, decl_acc) )
  in
  if List.is_empty defs then
    let msg = Fmt.strf "Cannot find any function definition in file %s" name in
    Result.Error msg
  else Result.Ok (defs, decls)

let rec translate_type c_type =
  let open Ctypes in
  match c_type with
  | Tvoid ->
      Logs.warn (fun m -> m "Encounter void type") ;
      Result.Ok Type.BoolType
  | Tint (size, _, _) -> (
    match size with
    | IBool -> Result.Ok Type.BoolType
    | _ -> Result.Ok Type.IntType )
  | Tlong _ -> Result.Ok Type.IntType
  | Tarray (base_ctype, _, _) | Tpointer (base_ctype, _) ->
      translate_arraytype [Type.IntType] base_ctype
  | Tfloat _ -> Result.Error "Cannot handle float type"
  | Tfunction _ -> Result.Error "Cannot handle function type"
  | Tstruct _ -> Result.Error "Cannot handle struct type"
  | Tunion _ -> Result.Error "Cannot handle struct type"

and translate_arraytype acc base_ctype =
  let open Result in
  match base_ctype with
  | Ctypes.Tarray (base_base_ctype, _, _) | Ctypes.Tpointer (base_base_ctype, _) ->
      translate_arraytype (Type.IntType :: acc) base_base_ctype
  | _ ->
      translate_type base_ctype
      >>= fun val_ty ->
      let key_tys = List.rev acc in
      Result.Ok (Type.ArrayType (key_tys, val_ty))

let translate_const t = function
  | Values.Vint i -> (
    match t with
    | Ctypes.Tint (sz, _, _) ->
        let i = Camlcoq.camlint_of_coqint i in
        let lit =
          if sz = Ctypes.IBool then
            let value = if i = Int32.zero then false else true in
            Literal.BoolLit value
          else Literal.IntLit (Bigint.of_int32 i)
        in
        Result.Ok lit
    | _ -> failwith "[INTERNAL] Int const cannot have non-int type" )
  | Values.Vlong i64 ->
      let i64 = Camlcoq.camlint64_of_coqint i64 in
      let lit = Literal.IntLit (Bigint.of_int64 i64) in
      Result.Ok lit
  | _ as c ->
      let msg =
        Fmt.strf "Cannot handle literal: %a" PrintCsyntax.print_value c
      in
      Result.Error msg

(* Return value meaning: new_bindings, helper_stmts, translated_expr *)
let rec translate_expr (env: Env.t) e =
  let open Result in
  let open Csyntax in
  match e with
  | Eval (c, t) ->
      translate_const t c >>= fun lit -> Ok ([], [], Expr.LiteralExpr lit)
  | Evar (id, _) -> (
    match Maps.PMap.get id env.var_map with
    | Some name -> Ok ([], [], Expr.VarExpr name)
    | None ->
        let msg = "Accessing an undefined variable" in
        Error msg )
  | Evalof (e, _) -> translate_expr env e
  | Eunop (op, e, _) -> translate_un_expr env op e
  | Ebinop (op, e1, e2, _) -> translate_bin_expr env op e1 e2
  | Eseqand (e1, e2, _) ->
      translate_simple_bin_expr env BinaryOperator.And e1 e2
  | Eseqor (e1, e2, _) -> translate_simple_bin_expr env BinaryOperator.Or e1 e2
  | Ederef (Ebinop (Cop.Oadd, e1, e2, _), _) -> translate_deref_expr env e1 e2
  | Ecast (e, _) -> translate_expr env e
  | Eparen (e, _, _) -> translate_expr env e
  | Econdition (ce, te, fe, _) ->
      translate_expr env ce
      >>= fun (l0, ss0, cond) ->
      translate_expr env te
      >>= fun (l1, ss1, true_val) ->
      translate_expr env fe
      >>= fun (l2, ss2, false_val) ->
      Ok
        ( List.concat [l0; l1; l2]
        , List.concat [ss0; ss1; ss2]
        , Expr.CondExpr {cond; true_val; false_val} )
  | Ecall (Evalof (Evar (id, _), _), args, retty) ->
      translate_type retty
      >>= fun ty ->
      translate_expr_list env args
      >>= fun (lss, sss, args) ->
      let name = IdentiferGenerator.get env.id_gen ~name:"tmpret" in
      let binding = VarBinding.{name; ty} in
      let fun_name = get_func_identifier env.side id in
      let stmt =
        Stmt.Call {rets= [Lvalue.of_var name]; name= fun_name; args}
      in
      Ok (binding :: lss, List.append sss [stmt], Expr.VarExpr name)
  | _ ->
      let msg = Fmt.strf "Cannot handle expr: %a" PrintCsyntax.print_expr e in
      Error msg

and translate_expr_list env exprs =
  let open Csyntax in
  let open Result in
  match exprs with
  | Enil -> Ok ([], [], [])
  | Econs (e, rest) ->
      translate_expr env e
      >>= fun (ls, ss, e) ->
      translate_expr_list env rest
      >>= fun (lrest, ssrest, erest) ->
      Ok (List.append ls lrest, List.append ss ssrest, e :: erest)

and translate_un_expr env op e =
  match op with
  | Cop.Oneg -> translate_simple_un_expr env UnaryOperator.Neg e
  | Cop.Onotbool -> translate_simple_un_expr env UnaryOperator.Not e
  | Cop.Onotint -> translate_funcall_expr env BitvecBuiltin.(ident_of BvNot) [e]
  | _ ->
      let msg = Fmt.strf "Cannot handle unary operator" in
      Result.Error msg

and translate_simple_un_expr env op e =
  let open Result in
  translate_expr env e
  >>= fun (ls, ss, e) -> Ok (ls, ss, Expr.UnaryExpr (op, e))

and translate_bin_expr env op e1 e2 =
  match op with
  | Cop.Oadd -> translate_simple_bin_expr env BinaryOperator.Plus e1 e2
  | Cop.Osub -> translate_simple_bin_expr env BinaryOperator.Minus e1 e2
  | Cop.Omul -> translate_simple_bin_expr env BinaryOperator.Mult e1 e2
  | Cop.Odiv -> translate_simple_bin_expr env BinaryOperator.Div e1 e2
  | Cop.Omod -> translate_simple_bin_expr env BinaryOperator.Mod e1 e2
  | Cop.Oeq -> translate_simple_bin_expr env BinaryOperator.Eq e1 e2
  | Cop.One -> translate_simple_bin_expr env BinaryOperator.Ne e1 e2
  | Cop.Olt -> translate_simple_bin_expr env BinaryOperator.Lt e1 e2
  | Cop.Ogt -> translate_simple_bin_expr env BinaryOperator.Gt e1 e2
  | Cop.Ole -> translate_simple_bin_expr env BinaryOperator.Le e1 e2
  | Cop.Oge -> translate_simple_bin_expr env BinaryOperator.Ge e1 e2
  | Cop.Oand ->
      translate_funcall_expr env BitvecBuiltin.(ident_of BvAnd) [e1; e2]
  | Cop.Oor -> translate_funcall_expr env BitvecBuiltin.(ident_of BvOr) [e1; e2]
  | Cop.Oxor ->
      translate_funcall_expr env BitvecBuiltin.(ident_of BvXor) [e1; e2]
  | Cop.Oshl -> (
    match e2 with
    | Csyntax.Eval (c, t) -> (
        let open Result in
        match translate_const t c with
        | Ok (Literal.IntLit i) ->
            translate_expr env e1
            >>= fun (ls, ss, lhs) ->
            let two_exp_i = Bigint.(of_int 2 ** i) in
            let rhs = Expr.LiteralExpr (Literal.IntLit two_exp_i) in
            Ok (ls, ss, Expr.BinaryExpr (BinaryOperator.Mult, lhs, rhs))
        | _ ->
            translate_funcall_expr env BitvecBuiltin.(ident_of BvShl) [e1; e2]
        )
    | _ -> translate_funcall_expr env BitvecBuiltin.(ident_of BvShl) [e1; e2] )
  | Cop.Oshr ->
    match e2 with
    | Csyntax.Eval (c, t) -> (
        let open Result in
        match translate_const t c with
        | Ok (Literal.IntLit i) ->
            translate_expr env e1
            >>= fun (ls, ss, lhs) ->
            let two_exp_i = Bigint.(of_int 2 ** i) in
            let rhs = Expr.LiteralExpr (Literal.IntLit two_exp_i) in
            Ok (ls, ss, Expr.BinaryExpr (BinaryOperator.Div, lhs, rhs))
        | _ ->
            translate_funcall_expr env BitvecBuiltin.(ident_of BvShr) [e1; e2]
        )
    | _ -> translate_funcall_expr env BitvecBuiltin.(ident_of BvShr) [e1; e2]

and translate_simple_bin_expr env op e1 e2 =
  let open Result in
  translate_expr env e1
  >>= fun (ls0, ss0, lhs) ->
  translate_expr env e2
  >>= fun (ls1, ss1, rhs) ->
  Ok (List.append ls0 ls1, List.append ss0 ss1, Expr.BinaryExpr (op, lhs, rhs))

and translate_funcall_expr env name args =
  let open Result in
  all (List.map args ~f:(translate_expr env))
  >>= fun res ->
  let lss, sss, args = List.unzip3 res in
  Ok (List.concat lss, List.concat sss, Expr.FunCallExpr {name; args})

and translate_deref_expr env e1 e2 =
  let rec translate_deref_impl acc e =
    match e with
    | Csyntax.Ederef (Csyntax.Ebinop (Cop.Oadd, e1, e2, _), _)
     |Csyntax.Evalof
        (Csyntax.Ederef (Csyntax.Ebinop (Cop.Oadd, e1, e2, _), _), _) ->
        let acc = e2 :: acc in
        translate_deref_impl acc e1
    | _ ->
        let open Result in
        translate_expr env e
        >>= fun (ls0, ss0, base) ->
        all (List.map (List.rev acc) ~f:(translate_expr env))
        >>= fun res ->
        let lsrest, ssrest, indices = List.unzip3 res in
        Ok
          ( List.concat (ls0 :: lsrest)
          , List.concat (ss0 :: ssrest)
          , Expr.ArraySelectExpr {base; indices} )
  in
  translate_deref_impl [e2] e1

let lval_from_expr = function
  | Expr.VarExpr v ->
      let l = Lvalue.{base= v; indices= []} in
      Result.Ok l
  | Expr.ArraySelectExpr {base= Expr.VarExpr v; indices} ->
      let l = Lvalue.{base= v; indices} in
      Result.Ok l
  | _ as e ->
      let msg = Fmt.strf "Expr cannot be used as lvalue: %a" Expr.pp e in
      Result.Error msg

let translate_lval env e =
  let open Result in
  translate_expr env e
  >>= fun (ls, ss, e) -> lval_from_expr e >>= fun lval -> Ok (ls, ss, lval)

let translate_binding id_gen env name cid cty =
  let open Result in
  let name = IdentiferGenerator.get id_gen ~name in
  translate_type cty
  >>= fun ty ->
  let binding = VarBinding.{name; ty} in
  let env' = Maps.PMap.set cid (Some name) env in
  Result.Ok (env', binding)

let translate_bindings id_gen env name vinfo_list =
  let open Result in
  let init = Ok (env, []) in
  List.fold vinfo_list ~init ~f:(fun acc (cid, cty) ->
      match acc with
      | Error _ -> acc
      | Ok (env, bds) ->
          translate_binding id_gen env name cid cty
          >>= fun (env', binding) -> Ok (env', binding :: bds) )
  >>= fun (env, rev_bindings) -> Ok (env, List.rev rev_bindings)

let rec translate_stmt env (stmt: Csyntax.statement) =
  let open Result in
  let open Csyntax in
  match stmt with
  | Sskip -> Result.Ok ([], [])
  | Sdo (Epostincr (iord, e, _)) ->
      translate_expr env e
      >>= fun (ls, ss, e) ->
      lval_from_expr e
      >>= fun lhs ->
      let rhs =
        match iord with
        | Cop.Incr ->
            Expr.BinaryExpr
              ( BinaryOperator.Plus
              , e
              , Expr.LiteralExpr (Literal.IntLit Bigint.one) )
        | Cop.Decr ->
            Expr.BinaryExpr
              ( BinaryOperator.Minus
              , e
              , Expr.LiteralExpr (Literal.IntLit Bigint.one) )
      in
      let stmt = Stmt.Assign {lhs; rhs} in
      Result.Ok (ls, List.append ss [stmt])
  | Sdo (Eassign (lhs, rhs, _)) ->
      translate_lval env lhs
      >>= fun (ls0, ss0, lhs) ->
      translate_expr env rhs
      >>= fun (ls1, ss1, rhs) ->
      let stmt = Stmt.Assign {lhs; rhs} in
      Result.Ok (List.append ls0 ls1, List.concat [ss0; ss1; [stmt]])
  | Sdo (Eassignop (bop, clhs, crhs, _, _)) ->
      translate_lval env clhs
      >>= fun (ls0, ss0, lhs) ->
      translate_bin_expr env bop clhs crhs
      >>= fun (ls1, ss1, rhs) ->
      let stmt = Stmt.Assign {lhs; rhs} in
      Result.Ok (List.append ls0 ls1, List.concat [ss0; ss1; [stmt]])
  | Sdo (Ecomma (e0, e1, _)) ->
      translate_stmt env (Sdo e0)
      >>= fun (l0, s0) ->
      translate_stmt env (Sdo e1)
      >>= fun (l1, s1) -> Result.Ok (List.append l0 l1, List.append s0 s1)
  | Sdo (Ecall (Evalof (Evar (id, _), _), args, _)) -> (
    match Maps.PMap.get id env.decl_map with
    | None ->
        let msg = "Unrecognized function call" in
        Result.Error msg
    | Some BuiltinExternal.Assume ->
      match args with
      | Enil ->
          let msg =
            "Cannot pass empty argument list to assume built-in call"
          in
          Result.Error msg
      | Econs (arg, _) ->
          translate_expr env arg
          >>= fun (locals, stmts, e) ->
          let stmt = Stmt.Assume e in
          Result.Ok (locals, List.append stmts [stmt]) )
  | Ssequence (s0, s1) ->
      translate_stmt env s0
      >>= fun (l0, stmts0) ->
      translate_stmt env s1
      >>= fun (l1, stmts1) ->
      let locals = List.append l0 l1 in
      let stmts = List.append stmts0 stmts1 in
      Result.Ok (locals, stmts)
  | Sifthenelse (e, tb, fb) ->
      translate_expr env e
      >>= fun (l0, ss, cond) ->
      translate_stmt env tb
      >>= fun (l1, then_branch) ->
      translate_stmt env fb
      >>= fun (l2, else_branch) ->
      let stmt = Stmt.If {cond; then_branch; else_branch} in
      Result.Ok (List.concat [l0; l1; l2], List.append ss [stmt])
  | Swhile (e, b) ->
      translate_expr env e
      >>= fun (l0, ss, cond) ->
      translate_stmt env b
      >>= fun (l1, body) ->
      let stmt = Stmt.While {cond; body} in
      Result.Ok (List.append l0 l1, List.append ss [stmt])
  | Sdowhile (e, b) ->
      translate_expr env e
      >>= fun (l0, ss, cond) ->
      translate_stmt env b
      >>= fun (l1, body) ->
      let branch_stmt = Stmt.If {cond; then_branch= body; else_branch= []} in
      let loop_stmt = Stmt.While {cond; body} in
      Result.Ok (List.append l0 l1, List.append ss [branch_stmt; loop_stmt])
  | Sfor (init_stmt, term_exp, update_stmt, body_stmt) ->
      translate_expr env term_exp
      >>= fun (l0, ss, cond) ->
      translate_stmt env init_stmt
      >>= fun (l1, istmts) ->
      translate_stmt env update_stmt
      >>= fun (l2, ustmts) ->
      translate_stmt env body_stmt
      >>= fun (l3, bstmts) ->
      let loop_stmt = Stmt.While {cond; body= List.append bstmts ustmts} in
      let stmts = List.concat [ss; istmts; [loop_stmt]] in
      let locals = List.concat [l0; l1; l2; l3] in
      Result.Ok (locals, stmts)
  | Sreturn opt_expr ->
      (* The correctness of this translation relies on the assumption that all returns must have no successor stmts *)
      (* TODO: Add a CSyntax prepass to move all return stmts to tail position *)
      let eres =
        match opt_expr with
        | Some e -> translate_expr env e
        | None -> Result.Ok ([], [], Expr.LiteralExpr (Literal.BoolLit false))
      in
      eres
      >>= fun (ls, ss, e) ->
      let assign_stmt =
        Stmt.Assign {lhs= Lvalue.{base= env.ret_name; indices= []}; rhs= e}
      in
      Result.Ok (ls, List.append ss [assign_stmt])
  | _ ->
      let msg =
        Fmt.strf "Cannot handle stmt: %a" PrintCsyntax.print_stmt stmt
      in
      Result.Error msg

let has_void_type = function Ctypes.Tvoid -> true | _ -> false

let translate_fun_def side name fenv (fun_def: Csyntax.coq_function) =
  let open Result in
  translate_type fun_def.fn_return
  >>= fun ret_ty ->
  let id_gen = IdentiferGenerator.create () in
  let ret_name = IdentiferGenerator.get id_gen ~name:"ret" in
  let rets =
    match has_void_type fun_def.fn_return with
    | true -> []
    | false -> [VarBinding.{name= ret_name; ty= ret_ty}]
  in
  let venv = Maps.PMap.init None in
  translate_bindings id_gen venv "param" fun_def.fn_params
  >>= fun (venv, params) ->
  translate_bindings id_gen venv "local" fun_def.fn_vars
  >>= fun (venv, locals) ->
  let env = Env.{var_map= venv; decl_map= fenv; id_gen; side; ret_name} in
  translate_stmt env fun_def.fn_body
  >>= fun (new_locals, stmts) ->
  let locals = List.append new_locals locals in
  let proc = Procedure.{name; params; rets; locals; stmts} in
  Result.Ok proc

let process_c_file config name side (c_file: C.program) =
  let open Result in
  C2C.convertProgram c_file
  |> of_option ~error:"Csyntax conversion failed"
  >>= fun csprog ->
  if config.TransConfig.print_csyntax then
    PrintCsyntax.print_program Fmt.stderr csprog ;
  extract_fun_defs side name csprog
  >>= fun (defs, decls) ->
  let fenv =
    List.fold ~init:(Maps.PMap.init None) decls ~f:(fun acc (id, decl) ->
        Maps.PMap.set id (Some decl) acc )
  in
  all
    (List.map defs ~f:(fun (name, fun_def) ->
         translate_fun_def side name fenv fun_def ))
  >>= fun procs ->
  match procs with
  | [] -> failwith "[INTERNAL] empty procedure list in a C source file"
  | s :: rest ->
      (* The last function in the source file is used as its entry point *)
      Ok (s, rest)

(* Find all params and locals which starts with name "bool" and change their type to bool type *)
(* Note that this fix only applies to programs that are generated by the random sampler *)
let retype_cprogram : C.program -> C.program =
  let is_bool_ident ident =
    let name = ident.C.name in
    String.is_prefix name ~prefix:"bool"
  in
  List.map ~f:(fun gdecl ->
      let gdesc =
        match gdecl.C.gdesc with
        | C.Gfundef fdef ->
            let fd_params =
              List.map fdef.C.fd_params ~f:(fun ((ident, _) as p) ->
                  if is_bool_ident ident then (ident, C.TInt (C.IBool, []))
                  else p )
            in
            let fd_locals =
              List.map fdef.C.fd_locals ~f:
                (fun ((s, ident, _, opt_init) as d) ->
                  if is_bool_ident ident then
                    let opt_init =
                      Option.map opt_init ~f:(fun i ->
                          match i with
                          | C.Init_single
                              {edesc= C.EConst (C.CInt (n, _, s)); _} ->
                              C.Init_single
                                { edesc= C.EConst (C.CInt (n, C.IBool, s))
                                ; etyp= C.TInt (C.IBool, []) }
                          | _ -> i )
                    in
                    (s, ident, C.TInt (C.IBool, []), opt_init)
                  else d )
            in
            C.Gfundef {fdef with fd_params; fd_locals}
        | _ as d -> d
      in
      {gdecl with gdesc} )

let translate_one_file (config: TransConfig.t) side file =
  let open Result in
  try
    let cprog = Parse.parse_c_file file file in
    let cprog = if config.retype_vars then retype_cprogram cprog else cprog in
    if config.print_c then Cprint.program Fmt.stderr cprog ;
    process_c_file config file side cprog
  with Assert_failure _ ->
    let msg = Fmt.strf "Compcert parser rejected the file %s" file in
    Error msg

let get_eq_spec (p0: Procedure.t) (p1: Procedure.t) =
  let params0 = p0.params in
  let rets0 = p0.rets in
  let params1 = p1.params in
  let rets1 = p1.rets in
  match (List.zip params0 params1, List.zip rets0 rets1) with
  | Some params, Some rets ->
      let create_eq_expr (bd0, bd1) =
        let name0 = bd0.VarBinding.name in
        let name1 = bd1.VarBinding.name in
        Expr.BinaryExpr
          ( BinaryOperator.Eq
          , Expr.AnnotatedExpr {annot= Side.Left; expr= Expr.VarExpr name0}
          , Expr.AnnotatedExpr {annot= Side.Right; expr= Expr.VarExpr name1} )
      in
      let requires = List.map params ~f:create_eq_expr in
      let param_ensures =
        List.map
          (List.filter params ~f:(fun (bd0, _) ->
               Type.is_array_type bd0.VarBinding.ty ))
          ~f:create_eq_expr
      in
      let ensures = List.map rets ~f:create_eq_expr in
      let ensures = List.append param_ensures ensures in
      let spec = Spec.{requires; ensures} in
      Result.Ok spec
  | _ ->
      let msg = "Procedure param/return number does not match" in
      Result.Error msg

let coeus_fix_type config ast =
  if config.TransConfig.fix_types then Frontend.TypeFixer.run ast
  else Result.Ok ast

let coeus_type_check config ast =
  let open Result in
  if config.TransConfig.type_check then
    Frontend.VariableRenamer.run ast
    >>= fun ast ->
    Frontend.Resolver.run ast
    >>= fun env -> Frontend.TypeChecker.check_prog env ast >>= fun _ -> Ok ast
  else Ok ast

let translate_pair config file0 file1 =
  let open Result in
  translate_one_file config Side.Left file0
  >>= fun (entry_proc0, procs0) ->
  translate_one_file config Side.Right file1
  >>= fun (entry_proc1, procs1) ->
  let entry = EntrySpec.{left= entry_proc0.name; right= entry_proc1.name} in
  get_eq_spec entry_proc0 entry_proc1
  >>= fun spec ->
  let decls = List.map BitvecBuiltin.all ~f:BitvecBuiltin.decl_of in
  let prog =
    { decls
    ; procs= List.concat [procs0; procs1; [entry_proc0; entry_proc1]]
    ; entry
    ; spec }
  in
  coeus_fix_type config prog >>= fun prog -> coeus_type_check config prog

let init () =
  (* Required setup for Compcert parser to work *)
  Machine.config := Machine.x86_64 ;
  Sections.initialize ()
