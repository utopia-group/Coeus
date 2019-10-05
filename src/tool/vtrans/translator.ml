open Core
open Ast.Ecoeus

exception TranslationError of string

let left_c_file = "1.c"

let right_c_file = "2.c"

let spec_file = "relprop"

let with_files ~f outdir =
  let with_file =
    Out_channel.with_file ~binary:false ~append:false ~fail_if_exists:false
  in
  let out_filename0 = Filename.concat outdir left_c_file in
  with_file out_filename0 ~f:(fun oc0 ->
      let out_filename1 = Filename.concat outdir right_c_file in
      with_file out_filename1 ~f:(fun oc1 ->
          let spec_filename = Filename.concat outdir spec_file in
          with_file spec_filename ~f:(fun ocs -> f oc0 oc1 ocs) ) )

module VarKind = struct
  type t = Param | Ret | Local

  let name_prefix_of = function Param -> "p" | Ret -> "r" | Local -> "l"

  let spec_prefix_of = function Param -> "P" | Ret -> "R" | Local -> "L"
end

module VarEnv = struct
  type t = {id_table: (VarKind.t * int) Identifier.Table.t}

  let get_var_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d" (VarKind.name_prefix_of vkind) vid

  let get_array_size_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d_size" (VarKind.name_prefix_of vkind) vid

  let get_spec_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d" (VarKind.spec_prefix_of vkind) vid

  let get_array_size_spec_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d_SIZE" (VarKind.spec_prefix_of vkind) vid

  let get_array_index_spec_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d_INDEX" (VarKind.spec_prefix_of vkind) vid

  let get_array_value_spec_name_exn {id_table; _} name =
    let vkind, vid = Identifier.Table.find_exn id_table name in
    Fmt.strf "%s%d_VALUE" (VarKind.spec_prefix_of vkind) vid

  let get_proc_name_exn _ name =
    let name_str = Identifier.string_of name in
    String.Search_pattern.(replace_all (create ".") ~in_:name_str ~with_:"_")

  let of_prog ast =
    let id_table = Identifier.Table.create () in
    let param_id = ref 0 in
    let ret_id = ref 0 in
    let local_id = ref 0 in
    let add_var vkind VarBinding.({name; _}) =
      let vid =
        match vkind with
        | VarKind.Param ->
            let id = !param_id in
            incr param_id ; id
        | VarKind.Ret ->
            let id = !ret_id in
            incr ret_id ; id
        | VarKind.Local ->
            let id = !local_id in
            incr local_id ; id
      in
      ignore (Identifier.Table.add id_table ~key:name ~data:(vkind, vid))
    in
    let process_proc (proc: Procedure.t) =
      List.iter proc.params ~f:(add_var VarKind.Param) ;
      List.iter proc.rets ~f:(add_var VarKind.Ret) ;
      List.iter proc.locals ~f:(add_var VarKind.Local)
    in
    List.iter ast.procs ~f:process_proc ;
    {id_table}
end

let pp_literal fmt lit =
  let open Literal in
  match lit with
  | BoolLit false -> Fmt.pf fmt "0"
  | BoolLit true -> Fmt.pf fmt "1"
  | IntLit i -> Fmt.pf fmt "%a" Bigint.pp i

let filter_arrays =
  List.filter ~f:(fun VarBinding.({ty; _}) -> not (Type.is_array_type ty))

let rec contains_identifier expr =
  let open Expr in
  match expr.bare_expr with
  | LiteralExpr _ -> false
  | VarExpr _ | FunCallExpr _ -> true
  | UnaryExpr (_, e) -> contains_identifier e
  | BinaryExpr (_, lhs, rhs) ->
      contains_identifier lhs || contains_identifier rhs
  | ArraySelectExpr {base; indices} ->
      contains_identifier base || List.exists ~f:contains_identifier indices
  | ArrayStoreExpr {base; indices; value} ->
      contains_identifier base
      || List.exists ~f:contains_identifier indices
      || contains_identifier value
  | CondExpr {cond; true_val; false_val} ->
      contains_identifier cond
      || contains_identifier true_val
      || contains_identifier false_val
  | QuantifiedExpr {body; _} -> contains_identifier body

let format_prog ofmt env entry procs =
  let get_var_name_exn VarBinding.({name; _}) =
    VarEnv.get_var_name_exn env name
  in
  let pp_binding fmt VarBinding.({name; ty}) =
    match ty with
    | Type.BoolType | Type.IntType ->
        Fmt.pf fmt "int %s" (VarEnv.get_var_name_exn env name)
    | Type.ArrayType ([Type.IntType], Type.IntType) ->
        Fmt.pf fmt "int %s, %s[%s]"
          (VarEnv.get_array_size_name_exn env name)
          (VarEnv.get_var_name_exn env name)
          (VarEnv.get_array_size_name_exn env name)
    | _ ->
        let msg =
          Fmt.strf "@[<h>Translation is not supported for this type: %a@]"
            Type.pp ty
        in
        raise (TranslationError msg)
  in
  let rec pp_expr fmt expr =
    let open Expr in
    match expr.bare_expr with
    | LiteralExpr lit -> Fmt.pf fmt "%a" pp_literal lit
    | VarExpr vb -> Fmt.pf fmt "%s" (get_var_name_exn vb)
    | UnaryExpr (op, e) -> Fmt.pf fmt "(%a %a)" UnaryOperator.pp op pp_expr e
    | BinaryExpr (op, lhs, rhs) -> (
        let open BinaryOperator in
        match op with
        | Plus | Minus | And | Or | Lt | Le | Gt | Ge | Eq | Ne ->
            Fmt.pf fmt "(%a %a %a)" pp_expr lhs BinaryOperator.pp op pp_expr
              rhs
        | Mult | Div ->
            if contains_identifier lhs && contains_identifier rhs then
              let msg =
                Fmt.strf "Nonlinear arithmetic is not supported: %a" Expr.pp
                  expr
              in
              raise (TranslationError msg)
            else
              Fmt.pf fmt "(%a %a %a)" pp_expr lhs BinaryOperator.pp op pp_expr
                rhs
        | _ ->
            let msg =
              Fmt.strf "Binary operator is not supported: %a" BinaryOperator.pp
                op
            in
            raise (TranslationError msg) )
    | ArraySelectExpr {base; indices} ->
        Fmt.pf fmt "%a[%a]" pp_expr base
          (Fmt.list ~sep:Fmt.comma pp_expr)
          indices
    | CondExpr {cond; true_val; false_val} ->
        Fmt.pf fmt "(%a ? %a : %a)" pp_expr cond pp_expr true_val pp_expr
          false_val
    | FunCallExpr _ | ArrayStoreExpr _ | QuantifiedExpr _ ->
        raise
          (TranslationError
             "Array store and quantified expressions are not supported")
  in
  let pp_lvalue fmt Lvalue.({base; indices; _} as lval) =
    Fmt.pf fmt "%s" (VarEnv.get_var_name_exn env base.name) ;
    match indices with
    | [] -> ()
    | [index] -> Fmt.pf fmt "[%a]" pp_expr index
    | _ ->
        let msg =
          Fmt.strf "Multi-dimensional array is not supported: %a" Lvalue.pp
            lval
        in
        raise (TranslationError msg)
  in
  let rec pp_stmt fmt stmt =
    let open Stmt in
    match stmt.bare_stmt with
    | Assume _ -> raise (TranslationError "Assume stmt is not supported")
    | Assign {lhs; rhs} ->
        Fmt.pf fmt "@[<h>%a = %a;@]" pp_lvalue lhs pp_expr rhs
    | If {cond; then_branch; else_branch} ->
        Fmt.pf fmt "@[<v 2>@[<h>if (%a) {@]@,%a" pp_expr cond pp_stmts
          then_branch ;
        if List.is_empty else_branch then Fmt.pf fmt "@]@,}"
        else Fmt.pf fmt "@]@,@[<v 2>} else {@,%a@]@,}" pp_stmts else_branch
    | While {cond; body} ->
        Fmt.pf fmt "@[<v 2>@[<h>while (%a) {@]@,%a@]@,}" pp_expr cond pp_stmts
          body
    | Call {rets; name; args} ->
        let _ =
          match rets with
          | [] -> ()
          | [lval] -> Fmt.pf fmt "@[<h>%a = " pp_lvalue lval
          | _ ->
              raise
                (TranslationError
                   "Call stmt with multiple return value is not supported")
        in
        Fmt.pf fmt "%s(%a);@]"
          (VarEnv.get_proc_name_exn env name)
          (Fmt.list ~sep:Fmt.comma pp_expr)
          (List.filter args ~f:(fun arg ->
               not (Type.is_array_type (Expr.type_of arg)) ))
    | For {counter; lower; upper; step; direction; body} ->
        let counter_str = VarEnv.get_var_name_exn env counter in
        match direction with
        | ForDirection.Forward ->
            Fmt.pf fmt
              "@[<v 2>@[<h>for (%s = %a; %s < %a; %s = %s + %a) {@]@,%a@]@,}"
              counter_str pp_expr lower counter_str pp_expr upper counter_str
              counter_str pp_expr step pp_stmts body
        | ForDirection.Backward ->
            Fmt.pf fmt
              "@[<v 2>@[<h>for (%s = %a; %s > %a; %s = %s - %a) {@]@,%a@]@,}"
              counter_str pp_expr lower counter_str pp_expr upper counter_str
              counter_str pp_expr step pp_stmts body
  and pp_stmts fmt stmts =
    Fmt.pf fmt "%a" (Fmt.list ~sep:Fmt.cut pp_stmt) stmts
  in
  let write_preamble (entry_proc: Procedure.t) other_procs =
    let param_rets = List.append entry_proc.params entry_proc.rets in
    List.iter param_rets ~f:(fun vb -> Fmt.pf ofmt "%a;@," pp_binding vb) ;
    List.iter (entry_proc :: other_procs) ~f:(fun proc ->
        List.iter proc.Procedure.locals ~f:(fun vb ->
            if Type.is_array_type vb.VarBinding.ty then
              Fmt.pf ofmt "%a;@," pp_binding vb ) ) ;
    let param_names = List.map entry_proc.params ~f:get_var_name_exn in
    let ret_names = List.map entry_proc.rets ~f:get_var_name_exn in
    let ret_size_names =
      List.filter_map entry_proc.params ~f:(fun VarBinding.({name; ty}) ->
          match ty with
          | Type.ArrayType _ -> Some (VarEnv.get_array_size_name_exn env name)
          | _ -> None )
    in
    let input_names = List.append ret_size_names param_names in
    let output_names = ret_names in
    Fmt.pf ofmt
      "@,@[<v \
       2>/*@,MAP_specification@,@,specvars(@[<h>[%a]@],@[<h>[%a]@]).@]@,*/@,@,"
      (Fmt.list ~sep:Fmt.comma Fmt.string)
      input_names
      (Fmt.list ~sep:Fmt.comma Fmt.string)
      output_names
  in
  let write_stmts stmts = Fmt.pf ofmt "%a" pp_stmts stmts in
  let write_local_decls locals =
    List.iter locals ~f:(fun vb ->
        if not (Type.is_array_type vb.VarBinding.ty) then
          Fmt.pf ofmt "%a;@," pp_binding vb )
  in
  let write_main (proc: Procedure.t) =
    Fmt.pf ofmt "@[<v 2>void main() {@," ;
    write_local_decls proc.locals ;
    write_stmts proc.stmts ;
    Fmt.pf ofmt "@]@,}@,"
  in
  let write_other (proc: Procedure.t) =
    let opt_ret =
      match proc.rets with
      | [] -> None
      | [vb] -> Some vb
      | _ ->
          raise
            (TranslationError
               "Multiple return variables on non-entry procedure is not \
                supported")
    in
    let pp_ret_type fmt = function
      | None -> Fmt.pf fmt "void"
      | Some VarBinding.({ty; _}) ->
        match ty with
        | Type.BoolType | Type.IntType -> Fmt.pf fmt "int"
        | _ ->
            raise
              (TranslationError
                 "Array return type on non-entry procedure is not supported")
    in
    Fmt.pf ofmt "@[<v 2>@[<h>%a %s(%a)@]{@," pp_ret_type opt_ret
      (VarEnv.get_proc_name_exn env proc.name)
      (Fmt.list ~sep:Fmt.comma pp_binding)
      (filter_arrays proc.params) ;
    write_local_decls proc.locals ;
    write_local_decls proc.rets ;
    write_stmts proc.stmts ;
    match opt_ret with
    | None -> ()
    | Some vb ->
        Fmt.pf ofmt "return %s;@," (get_var_name_exn vb) ;
        Fmt.pf ofmt "}@]@,"
  in
  Fmt.pf ofmt "@[<v 0>" ;
  write_preamble entry procs ;
  write_main entry ;
  List.iter procs ~f:write_other ;
  Fmt.pf ofmt "@]" ;
  Format.pp_print_flush ofmt ()

let translate_specs env spec lproc rproc =
  let get_spec_name_exn VarBinding.({name; _}) =
    VarEnv.get_spec_name_exn env name
  in
  let rec translate_prim_spec expr =
    let open Expr in
    let open VSpec in
    let is_int_op lhs rhs =
      Type.equal (Expr.type_of lhs) Type.IntType
      && Type.equal (Expr.type_of rhs) Type.IntType
    in
    match expr.bare_expr with
    | LiteralExpr (Literal.IntLit i) -> (
      match Bigint.to_int i with
      | Some i -> VExpr.IntLiteral i
      | None ->
          let msg = Fmt.strf "Integer literal is too large: %a" Bigint.pp i in
          failwith msg )
    | VarExpr vb -> VExpr.Var (get_spec_name_exn vb)
    | BinaryExpr (BinaryOperator.Plus, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Plus (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Minus, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Minus (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Mult, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Mult (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Div, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Div (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Eq, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Eq (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Lt, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Lt (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Le, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Le (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Gt, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Gt (translate_prim_spec lhs, translate_prim_spec rhs)
    | BinaryExpr (BinaryOperator.Ge, lhs, rhs) when is_int_op lhs rhs ->
        VExpr.Ge (translate_prim_spec lhs, translate_prim_spec rhs)
    | LiteralExpr _ | UnaryExpr _ | BinaryExpr _ | ArraySelectExpr _
     |ArrayStoreExpr _ | CondExpr _ | FunCallExpr _ | QuantifiedExpr _ ->
        let msg =
          Fmt.strf "Expression at specification position is not supported: %a"
            Expr.pp expr
        in
        raise (TranslationError msg)
  in
  let is_array_tys lty rty =
    let array_int_type = Type.ArrayType ([Type.IntType], Type.IntType) in
    Type.equal lty array_int_type && Type.equal rty array_int_type
  in
  let translate_require expr =
    let open Expr in
    match expr.bare_expr with
    | BinaryExpr
        ( BinaryOperator.Eq
        , Expr.({bare_expr= VarExpr VarBinding.({name= lname; ty= lty}); _})
        , Expr.({bare_expr= VarExpr VarBinding.({name= rname; ty= rty}); _}) )
      when is_array_tys lty rty ->
        let open VSpec.VExpr in
        [ Eq
            ( Var (VarEnv.get_spec_name_exn env lname)
            , Var (VarEnv.get_spec_name_exn env rname) ) ]
    | _ -> [translate_prim_spec expr]
  in
  let translate_ensure expr =
    let open VSpec in
    let build_array_cmp lname rname builder =
      let build_read_vexpr name =
        let base = VarEnv.get_spec_name_exn env name in
        let bound = VarEnv.get_array_size_spec_name_exn env name in
        let index = VarEnv.get_array_index_spec_name_exn env name in
        let value = VarEnv.get_array_value_spec_name_exn env name in
        VExpr.ArrayRead {base; bound; index; value}
      in
      let build_index_eq_vexpr name0 name1 =
        let index0 = VarEnv.get_array_index_spec_name_exn env name0 in
        let index1 = VarEnv.get_array_index_spec_name_exn env name1 in
        VExpr.Eq (VExpr.Var index0, VExpr.Var index1)
      in
      let lread = build_read_vexpr lname in
      let rread = build_read_vexpr rname in
      let ieq = build_index_eq_vexpr lname rname in
      let target =
        builder
          (VarEnv.get_array_value_spec_name_exn env lname)
          (VarEnv.get_array_value_spec_name_exn env rname)
      in
      [lread; rread; ieq; target]
    in
    let open Expr in
    match expr.bare_expr with
    | BinaryExpr
        ( BinaryOperator.Lt
        , Expr.({bare_expr= VarExpr VarBinding.({name= lname; ty= lty}); _})
        , Expr.({bare_expr= VarExpr VarBinding.({name= rname; ty= rty}); _}) )
      when is_array_tys lty rty ->
        build_array_cmp lname rname (fun l r ->
            VExpr.Ge (VExpr.Var r, VExpr.Plus (VExpr.Var l, VExpr.IntLiteral 1))
        )
    | BinaryExpr
        ( BinaryOperator.Le
        , Expr.({bare_expr= VarExpr VarBinding.({name= lname; ty= lty}); _})
        , Expr.({bare_expr= VarExpr VarBinding.({name= rname; ty= rty}); _}) )
      when is_array_tys lty rty ->
        build_array_cmp lname rname (fun l r ->
            VExpr.Ge (VExpr.Var r, VExpr.Var l) )
    | BinaryExpr
        ( BinaryOperator.Gt
        , Expr.({bare_expr= VarExpr VarBinding.({name= lname; ty= lty}); _})
        , Expr.({bare_expr= VarExpr VarBinding.({name= rname; ty= rty}); _}) )
      when is_array_tys lty rty ->
        build_array_cmp lname rname (fun l r ->
            VExpr.Ge (VExpr.Var l, VExpr.Plus (VExpr.Var r, VExpr.IntLiteral 1))
        )
    | BinaryExpr
        ( BinaryOperator.Ge
        , Expr.({bare_expr= VarExpr VarBinding.({name= lname; ty= lty}); _})
        , Expr.({bare_expr= VarExpr VarBinding.({name= rname; ty= rty}); _}) )
      when is_array_tys lty rty ->
        build_array_cmp lname rname (fun l r ->
            VExpr.Ge (VExpr.Var l, VExpr.Var r) )
    | _ -> [translate_prim_spec expr]
  in
  let flatten_conjuncts es =
    let rec impl acc = function
      | [] -> acc
      | e :: rest ->
          let open Expr in
          match e.bare_expr with
          | BinaryExpr (BinaryOperator.And, lhs, rhs) ->
              impl acc (lhs :: rhs :: rest)
          | _ -> impl (e :: acc) rest
    in
    List.rev (impl [] es)
  in
  let elim_equalities es =
    let rev_pairs, rev_others =
      List.fold es ~init:([], []) ~f:(fun (pacc, oacc) e ->
          let open Expr in
          match e.bare_expr with
          | BinaryExpr (BinaryOperator.Ne, lhs, rhs)
            when Type.equal Type.IntType (Expr.type_of lhs)
                 && Type.equal Type.IntType (Expr.type_of rhs) ->
              let e0 =
                Expr.mk_binary BinaryOperator.Ge lhs
                  (Expr.mk_binary BinaryOperator.Plus rhs
                     (Expr.mk_literal (Literal.of_int 1)))
              in
              let e1 =
                Expr.mk_binary BinaryOperator.Ge rhs
                  (Expr.mk_binary BinaryOperator.Plus lhs
                     (Expr.mk_literal (Literal.of_int 1)))
              in
              let pacc = (e0, e1) :: pacc in
              (pacc, oacc)
          | BinaryExpr (BinaryOperator.Ne, lhs, rhs)
            when Type.is_array_type (Expr.type_of lhs)
                 && Type.is_array_type (Expr.type_of rhs) ->
              (* Technically speaking this won't type check for arrays *)
              (* We postpone the work of fixing the type issues later to the vspec translation phase *)
              let e0 = Expr.mk_binary BinaryOperator.Gt lhs rhs in
              let e1 = Expr.mk_binary BinaryOperator.Gt rhs lhs in
              let pacc = (e0, e1) :: pacc in
              (pacc, oacc)
          | _ ->
              let oacc = e :: oacc in
              (pacc, oacc) )
    in
    List.fold rev_pairs ~init:[List.rev rev_others] ~f:(fun acc (e0, e1) ->
        List.concat_map acc ~f:(fun rest -> [e0 :: rest; e1 :: rest]) )
  in
  let split_ensure es =
    es |> flatten_conjuncts
    |> List.map ~f:Expr.logical_negate_of
    |> elim_equalities
  in
  let entry_binding_of side proc =
    let open VSpec in
    let kind =
      match side with
      | Side.Left -> VPred.Kind.NewLeft
      | Side.Right -> VPred.Kind.NewRight
    in
    let param_rets = List.append proc.Procedure.params proc.rets in
    let params = List.map param_rets ~f:get_spec_name_exn in
    let array_sizes =
      List.filter_map proc.params ~f:(fun VarBinding.({name; ty}) ->
          match ty with
          | Type.ArrayType _ ->
              Some (VarEnv.get_array_size_spec_name_exn env name)
          | _ -> None )
    in
    let params = List.append array_sizes params in
    VPred.{kind; params}
  in
  let require_vconds =
    List.concat_map spec.Spec.requires ~f:translate_require
  in
  let vbindings =
    let left_binding = entry_binding_of Side.Left lproc in
    let right_binding = entry_binding_of Side.Right rproc in
    [left_binding; right_binding]
  in
  let ensuress = split_ensure spec.ensures in
  List.map ensuress ~f:(fun ensures ->
      let ensure_vconds = List.concat_map ensures ~f:translate_ensure in
      let free_vars =
        Expr.free_vars_of_exprs (List.append spec.requires ensures)
      in
      let val_vconds =
        if
          List.exists free_vars ~f:(fun VarBinding.({ty; _}) ->
              Type.is_array_type ty )
        then
          List.concat_map free_vars ~f:(fun VarBinding.({name; ty}) ->
              let prog_var = VarEnv.get_var_name_exn env name in
              let spec_var = VarEnv.get_spec_name_exn env name in
              let val_cond = VSpec.VExpr.Val {prog_var; spec_var} in
              match ty with
              | Type.ArrayType _ ->
                  let prog_var = VarEnv.get_array_size_name_exn env name in
                  let spec_var =
                    VarEnv.get_array_size_spec_name_exn env name
                  in
                  let size_val_cond = VSpec.VExpr.Val {prog_var; spec_var} in
                  [val_cond; size_val_cond]
              | _ -> [val_cond] )
        else []
      in
      let vconds = List.concat [require_vconds; ensure_vconds; val_vconds] in
      VSpec.{bindings= vbindings; conds= vconds} )

let format_to_string ~f =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt ;
  Format.pp_print_flush fmt () ;
  Buffer.contents buf

let write_output outroot idx lcontent rcontent spec =
  let outdir_name = Fmt.strf "sub%d" idx in
  let outdir = Filename.concat outroot outdir_name in
  (* With mkdir_p rather than mkdir we can avoid catching the exception if the directory already exists *)
  Unix.mkdir_p outdir ;
  with_files outdir ~f:(fun oc0 oc1 ocs ->
      Out_channel.output_string oc0 lcontent ;
      Out_channel.output_string oc1 rcontent ;
      Out_channel.output_string ocs spec )

let translate ast outdir =
  match ArrayRenamer.run ast with
  | Result.Error msg ->
      let msg = Fmt.strf "ArrayRenamer failed: %s" msg in
      raise (TranslationError msg)
  | Result.Ok ast ->
      let left_entry_proc, left_procs, right_entry_proc, right_procs =
        ArrayRenamer.split_procs ast
      in
      let env = VarEnv.of_prog ast in
      let lcontent =
        format_to_string ~f:(fun fmt ->
            format_prog fmt env left_entry_proc left_procs )
      in
      let rcontent =
        format_to_string ~f:(fun fmt ->
            format_prog fmt env right_entry_proc right_procs )
      in
      let specs =
        let specs' =
          translate_specs env ast.spec left_entry_proc right_entry_proc
        in
        List.map specs' ~f:(fun spec ->
            format_to_string ~f:(fun fmt -> VSpec.pp fmt spec) )
      in
      List.iteri specs ~f:(fun idx spec ->
          write_output outdir idx lcontent rcontent spec ) ;
      ()
