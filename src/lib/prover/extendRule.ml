open Core
open Ast.Ecoeus

(* This rule is z3-specific: we've found that replacing constant loop bounds with a variable loop bound can sometime speed up solving significantly *)

let extend_bound () =
  let bound_var = FreshNameGenerator.get_fresh_var () in
  let bound_binding = VarBinding.{name= bound_var; ty= Type.IntType} in
  bound_binding

let apply ast env _ (goal: Goal.t) =
  match (goal.left_stmts, goal.right_stmts) with
  | ( Stmt.({ bare_stmt=
                For
                  { counter= counter0
                  ; lower= lower0
                  ; upper= upper0
                  ; step= step0
                  ; direction= direction0
                  ; body= body0 }; _ } as s0)
      :: left_stmts
    , Stmt.({ bare_stmt=
                For
                  { counter= counter1
                  ; lower= lower1
                  ; upper= upper1
                  ; step= step1
                  ; direction= direction1
                  ; body= body1 }; _ } as s1)
      :: right_stmts )
    when Expr.equal (Expr.simplify lower0) (Expr.simplify lower1)
         && Expr.equal (Expr.simplify upper0) (Expr.simplify upper1)
         && Expr.equal (Expr.simplify step0) (Expr.simplify step1)
         && Stmt.ForDirection.equal direction0 direction1 ->
      let parent0 = Stmt.parent_of s0 in
      let parent1 = Stmt.parent_of s1 in
      let bound_v0 = extend_bound () in
      let bound_v1 = extend_bound () in
      let stmt0 =
        Stmt.mk_for parent0 counter0 lower0 (Expr.mk_var bound_v0) step0
          direction0 body0
      in
      let stmt1 =
        Stmt.mk_for parent1 counter1 lower1 (Expr.mk_var bound_v1) step1
          direction1 body1
      in
      let eq_cond =
        Expr.mk_binary BinaryOperator.Eq (Expr.mk_var bound_v0)
          (Expr.mk_var bound_v1)
      in
      let left_stmts = stmt0 :: left_stmts in
      let right_stmts = stmt1 :: right_stmts in
      let fv_cond = Verifier.Precondition.mk_havoc [bound_v0; bound_v1] in
      let pre_cond = Verifier.Precondition.mk_assume [eq_cond] in
      let pre_conds = pre_cond :: fv_cond :: goal.pre_conds in
      let ast = RuleHelper.insert_locals ast goal.left_proc [bound_v0] in
      let ast = RuleHelper.insert_locals ast goal.right_proc [bound_v1] in
      let goal = Goal.replace goal ~left_stmts ~right_stmts ~pre_conds in
      Some ([goal], env, ast)
  | _, _ -> None

let extend = QuasiLocalRule.mk_rule "extend" apply
