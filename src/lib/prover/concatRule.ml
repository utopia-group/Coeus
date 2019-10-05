open Core
open Ast.Ecoeus

let concat_for_stmts parent counter0 lower0 upper0 direction0 body0 counter1
    lower1 upper1 direction1 body1 =
  let range_expr lower upper direction =
    let lower', upper' =
      match direction with
      | Stmt.ForDirection.Forward -> (lower, upper)
      | Stmt.ForDirection.Backward -> (upper, lower)
    in
    Expr.simplify (Expr.mk_binary BinaryOperator.Minus upper' lower')
  in
  let set_counter_stmt counter lower iter_expr direction =
    let op =
      match direction with
      | Stmt.ForDirection.Forward -> BinaryOperator.Plus
      | Stmt.ForDirection.Backward -> BinaryOperator.Minus
    in
    let rhs = Expr.simplify (Expr.mk_binary op lower iter_expr) in
    Stmt.mk_assign parent
      (Lvalue.mk_var VarBinding.{name= counter; ty= Type.IntType})
      rhs
  in
  let new_counter = FreshNameGenerator.get_fresh_var () in
  let new_binding = VarBinding.{name= new_counter; ty= Type.IntType} in
  let erange0 = range_expr lower0 upper0 direction0 in
  let erange1 = range_expr lower1 upper1 direction1 in
  let total_range =
    Expr.simplify (Expr.mk_binary BinaryOperator.Plus erange0 erange1)
  in
  let cnt_init_stmt0 =
    set_counter_stmt counter0 lower0 (Expr.mk_var new_binding) direction0
  in
  let cnt_init_stmt1 =
    set_counter_stmt counter1 lower1
      (Expr.simplify
         (Expr.mk_binary BinaryOperator.Minus (Expr.mk_var new_binding) erange0))
      direction0
  in
  let cond =
    Expr.mk_binary BinaryOperator.Lt (Expr.mk_var new_binding) erange0
  in
  let dispatch_stmt =
    Stmt.mk_if parent cond (cnt_init_stmt0 :: body0) (cnt_init_stmt1 :: body1)
  in
  let for_stmt =
    Stmt.mk_for parent new_counter
      (Expr.mk_literal (Literal.IntLit Bigint.zero))
      total_range
      (Expr.mk_literal (Literal.IntLit Bigint.one))
      Stmt.ForDirection.Forward [dispatch_stmt]
  in
  (for_stmt, new_binding)

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with
  | Stmt.({ bare_stmt=
              For
                { step= Expr.({bare_expr= LiteralExpr (Literal.IntLit s0); _}); _
                }; _ })
    :: Stmt.({ bare_stmt=
                 For
                   { step=
                       Expr.({bare_expr= LiteralExpr (Literal.IntLit s1); _}); _
                   }; _ })
       :: _
    when s0 = Bigint.one && s1 = Bigint.one ->
      true
  | _ -> false

let apply_left ast env _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({ bare_stmt=
              For
                { counter= c0
                ; lower= l0
                ; upper= u0
                ; step= Expr.({bare_expr= LiteralExpr (Literal.IntLit s0); _})
                ; direction= d0
                ; body= b0 }; _ })
    :: Stmt.({ bare_stmt=
                 For
                   { counter= c1
                   ; lower= l1
                   ; upper= u1
                   ; step=
                       Expr.({bare_expr= LiteralExpr (Literal.IntLit s1); _})
                   ; direction= d1
                   ; body= b1 }; _ })
       :: rest
    when s0 = Bigint.one && s1 = Bigint.one ->
      let for_stmt, (cnt_binding: VarBinding.t) =
        concat_for_stmts goal.left_proc c0 l0 u0 d0 b0 c1 l1 u1 d1 b1
      in
      let left_stmts = for_stmt :: rest in
      let pre_conds =
        Verifier.Precondition.mk_havoc [cnt_binding] :: goal.pre_conds
      in
      let goal' = Goal.replace goal ~left_stmts ~pre_conds in
      let ast = RuleHelper.insert_locals ast goal.left_proc [cnt_binding] in
      Some ([goal'], env, ast)
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_quasilocal_rule ~apply_left side in
  QuasiLocalRule.mk_rule ~is_applicable:(is_applicable side) ~name apply

let concat_l = create "concat_l" Side.Left

let concat_r = create "concat_r" Side.Right
