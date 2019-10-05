open Core
open Coeus

let pp_var_binding fmt VarBinding.({name; ty}) =
  Fmt.pf fmt "%a %a" Type.pp ty Identifier.pp name

let pp_params = Fmt.parens (Fmt.list pp_var_binding ~sep:Fmt.comma)

let rec pp_expr fmt expr =
  let open Expr in
  match expr with
  | LiteralExpr l -> Literal.pp fmt l
  | VarExpr v -> Identifier.pp fmt v
  | UnaryExpr (op, e) -> Fmt.pf fmt "(%a %a)" UnaryOperator.pp op pp_expr e
  | BinaryExpr (op, lhs, rhs) ->
      Fmt.pf fmt "(%a %a %a)" pp_expr lhs BinaryOperator.pp op pp_expr rhs
  | ArraySelectExpr {base; indices} ->
      Fmt.pf fmt "%a[%a]" pp_expr base pp_expr_comma_sep_list indices
  | ArrayStoreExpr {base; indices; value} ->
      Fmt.pf fmt "%a[%a = %a]" pp_expr base pp_expr_comma_sep_list indices
        pp_expr value
  | AnnotatedExpr {annot; expr} ->
      Fmt.pf fmt "%a(%a)" Side.pp annot pp_expr expr
  | CondExpr {cond; true_val; false_val} ->
      Fmt.pf fmt "(if %a then %a else %a)" pp_expr cond pp_expr true_val
        pp_expr false_val
  | FunCallExpr {name; args} ->
      Fmt.pf fmt "%a(%a)" Identifier.pp name pp_expr_comma_sep_list args
  | QuantifiedExpr {quantifier; bindings; body} ->
      let pp_bindings = Fmt.list VarBinding.pp ~sep:Fmt.comma in
      Fmt.pf fmt "(%a %a ::@ %a)" Quantifier.pp quantifier pp_bindings bindings
        pp_expr body

and pp_expr_comma_sep_list fmt exprs =
  Fmt.list pp_expr ~sep:Fmt.comma fmt exprs

let rec pp_lvalue fmt Lvalue.({base; indices}) =
  if List.is_empty indices then Identifier.pp fmt base
  else
    Fmt.pf fmt "%a" pp_expr
      (Expr.ArraySelectExpr {base= Expr.VarExpr base; indices})

and pp_lvalue_comma_sep_list fmt lvalues =
  Fmt.list pp_lvalue ~sep:Fmt.comma fmt lvalues

let pp_for_step fmt = function
  | Expr.LiteralExpr (Literal.IntLit i) when Bigint.equal i Bigint.one -> ()
  | _ as e -> Fmt.pf fmt " step %a" Expr.pp e

let pp_direction fmt = function
  | Stmt.ForDirection.Forward -> Fmt.pf fmt "to"
  | Stmt.ForDirection.Backward -> Fmt.pf fmt "downto"

let rec pp_stmt fmt stmt =
  let open Stmt in
  match stmt with
  | Assume expr -> Fmt.pf fmt "@[<1>assume(%a);@]" pp_expr expr
  | Assign {lhs; rhs} -> Fmt.pf fmt "@[<1>%a = %a;@]" pp_lvalue lhs pp_expr rhs
  | If {cond; then_branch; else_branch} ->
      let pp_else_stmts fmt stmts =
        if List.is_empty stmts then ()
        else Fmt.pf fmt "@,else@,@[<v 2>{@,%a@]@,}" pp_stmts else_branch
      in
      Fmt.pf fmt "@[<1>if (%a)@]@,@[<v 2>{@,%a@]@,}%a" pp_expr cond pp_stmts
        then_branch pp_else_stmts else_branch
  | While {cond; body} ->
      Fmt.pf fmt "@[<1>while (%a)@]@,@[<v 2>{@,%a@]@,}" pp_expr cond pp_stmts
        body
  | For {counter; lower; upper; step; direction; body} ->
      Fmt.pf fmt "@[<1>for (%a = %a %a %a%a)@]@,@[<v 2>{@,%a@]@,}"
        Identifier.pp counter pp_expr lower pp_direction direction pp_expr
        upper pp_for_step step pp_stmts body
  | Call {rets; name; args} ->
      Fmt.pf fmt "@[<1>call %a = %a(%a);@]" pp_lvalue_comma_sep_list rets
        Identifier.pp name pp_expr_comma_sep_list args

and pp_stmts fmt stmts = Fmt.list pp_stmt fmt stmts

let pp_local fmt binding = Fmt.pf fmt "@[<h>%a;@]" pp_var_binding binding

let pp_proc_body fmt (locals, stmts) =
  let pp_locals =
    if List.is_empty locals then Fmt.nop
    else Fmt.suffix Fmt.cut (Fmt.list pp_local)
  in
  Fmt.pf fmt "@.@[<v 2>{@,%a%a@]@.}" pp_locals locals pp_stmts stmts

let pp_proc fmt Procedure.({name; params; rets; locals; stmts}) =
  Fmt.pf fmt "@[<1>procedure %a%a@ returns@ %a@]%a" Identifier.pp name
    pp_params params pp_params rets pp_proc_body (locals, stmts)

let pp_decl fmt FunDecl.({name; param_tys; ret_ty}) =
  Fmt.pf fmt "@[<1>declare %a %a(@,%a);@]" Type.pp ret_ty Identifier.pp name
    (Fmt.list ~sep:Fmt.comma Type.pp)
    param_tys

let pp_entry fmt EntrySpec.({left; right}) =
  Fmt.pf fmt "$lentry %a;@.$rentry %a;@." Identifier.pp left Identifier.pp
    right

let pp_spec fmt Spec.({requires; ensures}) =
  let spec_sep fmt () = Fmt.pf fmt "@." in
  let pp_require fmt e = Fmt.pf fmt "requires %a;" pp_expr e in
  let pp_ensure fmt e = Fmt.pf fmt "ensures %a;" pp_expr e in
  let pp_requires = Fmt.list ~sep:spec_sep pp_require in
  let pp_ensures = Fmt.list ~sep:spec_sep pp_ensure in
  Fmt.pf fmt "%a@.%a@." pp_requires requires pp_ensures ensures

let pp fmt {procs; decls; spec; entry} =
  let proc_sep fmt () = Fmt.pf fmt "@.@." in
  let decl_sep fmt () = Fmt.pf fmt "@." in
  let pp_decls = Fmt.list ~sep:decl_sep pp_decl in
  let pp_procs = Fmt.list ~sep:proc_sep pp_proc in
  Fmt.pf fmt "%a@.@.%a@.@.%a%a" pp_decls decls pp_procs procs pp_entry entry
    pp_spec spec
