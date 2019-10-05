open Core
open Ast.Ecoeus
open Verifier

let encode_literal Z3Env.({ctx; _}) = function
  | Literal.BoolLit b -> Z3.Boolean.mk_val ctx b
  | Literal.IntLit i ->
      Z3.Arithmetic.Integer.mk_numeral_s ctx (Bigint.to_string i)

let rec encode_type Z3Env.({ctx; _} as env) = function
  | Type.BoolType -> Z3.Boolean.mk_sort ctx
  | Type.IntType -> Z3.Arithmetic.Integer.mk_sort ctx
  | Type.ArrayType (key_tys, val_ty) ->
      let vsort = encode_type env val_ty in
      let rec build acc = function
        | [] -> acc
        | key_ty :: rest ->
            let ksort = encode_type env key_ty in
            let acc = Z3.Z3Array.mk_sort ctx ksort acc in
            build acc rest
      in
      build vsort (List.rev key_tys)

let encode_var Z3Env.({ctx; _} as env) VarBinding.({name; ty}) =
  let zsort = encode_type env ty in
  Z3.Expr.mk_const_s ctx (Identifier.string_of name) zsort

let encode_unary Z3Env.({ctx; _}) op e =
  let open UnaryOperator in
  match op with
  | Neg -> Z3.Arithmetic.mk_unary_minus ctx e
  | Not -> Z3.Boolean.mk_not ctx e

let encode_binary Z3Env.({ctx; _}) op lhs rhs =
  let open BinaryOperator in
  let open Z3 in
  match op with
  | Plus -> Arithmetic.mk_add ctx [lhs; rhs]
  | Minus -> Arithmetic.mk_sub ctx [lhs; rhs]
  | Mult -> Arithmetic.mk_mul ctx [lhs; rhs]
  | Div -> Arithmetic.mk_div ctx lhs rhs
  | Mod -> Arithmetic.Integer.mk_mod ctx lhs rhs
  | And -> Boolean.mk_and ctx [lhs; rhs]
  | Or -> Boolean.mk_or ctx [lhs; rhs]
  | Imply -> Boolean.mk_implies ctx lhs rhs
  | Lt -> Arithmetic.mk_lt ctx lhs rhs
  | Le -> Arithmetic.mk_le ctx lhs rhs
  | Gt -> Arithmetic.mk_gt ctx lhs rhs
  | Ge -> Arithmetic.mk_ge ctx lhs rhs
  | Eq -> Boolean.mk_eq ctx lhs rhs
  | Ne -> Boolean.mk_not ctx (Boolean.mk_eq ctx lhs rhs)

let encode_cond Z3Env.({ctx; _}) cond true_val false_val =
  Z3.Boolean.mk_ite ctx cond true_val false_val

let encode_array_select Z3Env.({ctx; _}) base indices =
  let rec impl acc = function
    | [] -> acc
    | index :: rest ->
        let acc = Z3.Z3Array.mk_select ctx acc index in
        impl acc rest
  in
  impl base indices

let encode_array_store Z3Env.({ctx; _}) base indices value =
  let _, rev_list =
    List.fold indices ~init:(base, []) ~f:(fun (base, acc) index ->
        let elem = (base, index) in
        let acc = elem :: acc in
        let base = Z3.Z3Array.mk_select ctx base index in
        (base, acc) )
  in
  List.fold rev_list ~init:value ~f:(fun acc (base, index) ->
      Z3.Z3Array.mk_store ctx base index acc )

let encode_fundecl Z3Env.({ctx; _} as env) FunDecl.({name; param_tys; ret_ty}) =
  let param_sorts = List.map param_tys ~f:(encode_type env) in
  let ret_sort = encode_type env ret_ty in
  Z3.FuncDecl.mk_func_decl_s ctx
    (Identifier.string_of name)
    param_sorts ret_sort

let encode_funcall Z3Env.({ctx; _}) fundecl args =
  Z3.Expr.mk_app ctx fundecl args

let encode_quantifier Z3Env.({ctx; _}) is_forall bindings body =
  let zquant =
    Z3.Quantifier.mk_quantifier ctx is_forall bindings body None [] [] None
      None
  in
  Z3.Quantifier.expr_of_quantifier zquant

let rec encode_expr env expr =
  let open Expr in
  match expr.bare_expr with
  | LiteralExpr lit -> encode_literal env lit
  | VarExpr vb -> encode_var env vb
  | UnaryExpr (op, e) ->
      let ze = encode_expr env e in
      encode_unary env op ze
  | BinaryExpr (op, lhs, rhs) ->
      let zlhs = encode_expr env lhs in
      let zrhs = encode_expr env rhs in
      encode_binary env op zlhs zrhs
  | ArraySelectExpr {base; indices} ->
      let zbase = encode_expr env base in
      let zindices = List.map indices ~f:(encode_expr env) in
      encode_array_select env zbase zindices
  | ArrayStoreExpr {base; indices; value} ->
      let zbase = encode_expr env base in
      let zindices = List.map indices ~f:(encode_expr env) in
      let zvalue = encode_expr env value in
      encode_array_store env zbase zindices zvalue
  | CondExpr {cond; true_val; false_val} ->
      let zcond = encode_expr env cond in
      let ztrue_val = encode_expr env true_val in
      let zfalse_val = encode_expr env false_val in
      encode_cond env zcond ztrue_val zfalse_val
  | FunCallExpr {func; args} ->
      let zfunc = encode_fundecl env func in
      let zargs = List.map args ~f:(encode_expr env) in
      encode_funcall env zfunc zargs
  | QuantifiedExpr {quantifier; bindings; body} ->
      let is_forall =
        match quantifier with
        | Quantifier.ForAll -> true
        | Quantifier.Exists -> false
      in
      (* This is not ideal as it pollutes the global namespace. However, we don't use quantifiers that much so for now it should suffice *)
      let bindings = List.map bindings ~f:(encode_var env) in
      let body = encode_expr env body in
      encode_quantifier env is_forall bindings body
