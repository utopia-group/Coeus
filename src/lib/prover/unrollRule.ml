open Core
open Ast.Ecoeus

let is_applicable side _ (goal: Goal.t) =
  let stmts =
    match side with
    | Side.Left -> goal.left_stmts
    | Side.Right -> goal.right_stmts
  in
  match stmts with stmt :: _ when Stmt.is_loop stmt -> true | _ -> false

let rec unroll_while acc parent cond body = function
  | 0 -> acc
  | n ->
      let if_stmt = Stmt.mk_if parent cond (List.append body acc) [] in
      let n = n - 1 in
      unroll_while [if_stmt] parent cond body n

let rec unroll_for acc acc_counter parent counter lower upper step direction
    body = function
  | 0 -> acc
  | n ->
      let subst_expr =
        match direction with
        | Stmt.ForDirection.Forward ->
            Expr.simplify (Expr.mk_binary BinaryOperator.Plus acc_counter step)
        | Stmt.ForDirection.Backward ->
            Expr.simplify
              (Expr.mk_binary BinaryOperator.Minus acc_counter step)
      in
      let subst_map = Identifier.Map.singleton counter subst_expr in
      let body' =
        List.map body
          ~f:(Stmt.map_expr ~f:(Expr.subst ~f:(Identifier.Map.find subst_map)))
      in
      let second_cond =
        match direction with
        | Stmt.ForDirection.Forward ->
            Expr.simplify (Expr.mk_binary BinaryOperator.Lt subst_expr upper)
        | Stmt.ForDirection.Backward ->
            Expr.simplify (Expr.mk_binary BinaryOperator.Gt subst_expr upper)
      in
      let body' =
        match second_cond.bare_expr with
        | Expr.LiteralExpr (Literal.BoolLit true) -> body'
        | Expr.LiteralExpr (Literal.BoolLit false) -> []
        | _ -> [Stmt.mk_if parent second_cond body' []]
      in
      let acc = List.append acc body' in
      let n = n - 1 in
      unroll_for acc subst_expr parent counter lower upper step direction body
        n

let apply_left num _ _ (goal: Goal.t) =
  if num <= 0 then None
  else
    match goal.left_stmts with
    | Stmt.({bare_stmt= While {cond; body}; _} as s0) :: rest ->
        let parent = Stmt.parent_of s0 in
        let if_stmts = unroll_while [] parent cond body num in
        let while_stmt =
          Stmt.mk_while parent cond (List.append body if_stmts)
        in
        let left_stmts = while_stmt :: rest in
        let goal' = Goal.replace goal ~left_stmts in
        Some goal'
    | Stmt.({bare_stmt= For {counter; lower; upper; step; direction; body}; _}
            as s0)
      :: rest ->
        let parent = Stmt.parent_of s0 in
        let rec build_step acc = function
          | 0 -> acc
          | n ->
              let acc = Expr.mk_binary BinaryOperator.Plus acc step in
              let n = n - 1 in
              build_step acc n
        in
        let step' = Expr.simplify (build_step step num) in
        let body' =
          unroll_for []
            (Expr.mk_var VarBinding.{name= counter; ty= Type.IntType})
            parent counter lower upper step direction body num
        in
        let new_body = List.append body body' in
        let for_stmt =
          Stmt.mk_for parent counter lower upper step' direction new_body
        in
        let left_stmts = for_stmt :: rest in
        let goal' = Goal.replace goal ~left_stmts in
        Some goal'
    | _ -> None

let create name side len =
  let apply =
    RuleHelper.mk_symmetric_local_rule ~apply_left:(apply_left len) side
  in
  LocalRule.mk_rule ~is_applicable:(is_applicable side) ~name apply

let unroll_l = create "unroll_l" Side.Left 1

let unroll_r = create "unroll_r" Side.Right 1
