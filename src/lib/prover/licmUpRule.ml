open Core
open Ast.Ecoeus

let is_disjoint s0 s1 =
  let inter = Hash_set.inter s0 s1 in
  Hash_set.is_empty inter

let can_lift_up evars stmt body_rest =
  let stmt_writes = Stmt.write_var_set_of stmt in
  let stmt_reads = Stmt.read_var_set_of stmt in
  let rest_writes = Stmts.write_var_set_of body_rest in
  is_disjoint evars stmt_writes
  && is_disjoint evars stmt_reads
  && is_disjoint stmt_writes rest_writes
  && is_disjoint stmt_writes rest_writes

let apply_left _ _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({bare_stmt= While {cond; body= stmt :: body_rest}; _} as s) :: rest ->
      let parent = Stmt.parent_of s in
      let evars = Expr.free_var_set_of cond in
      if can_lift_up evars stmt body_rest then
        let new_while = Stmt.mk_while parent cond body_rest in
        let if_stmt = Stmt.mk_if parent cond [stmt] [] in
        let left_stmts = if_stmt :: new_while :: rest in
        let goal' = Goal.replace goal ~left_stmts in
        Some goal'
      else None
  | Stmt.({ bare_stmt=
              For
                { counter
                ; lower
                ; upper
                ; step
                ; direction
                ; body= stmt :: body_rest; _ }; _ } as s)
    :: rest ->
      let parent = Stmt.parent_of s in
      let evars =
        Expr.free_var_set_of_exprs
          [ Expr.mk_var VarBinding.{name= counter; ty= Type.IntType}
          ; lower
          ; upper
          ; step ]
      in
      if can_lift_up evars stmt body_rest then
        let new_for =
          Stmt.mk_for parent counter lower upper step direction body_rest
        in
        let bop =
          match direction with
          | Stmt.ForDirection.Forward -> BinaryOperator.Lt
          | Stmt.ForDirection.Backward -> BinaryOperator.Gt
        in
        let cond = Expr.mk_binary bop lower upper in
        let if_stmt = Stmt.mk_if parent cond [stmt] [] in
        let left_stmts = if_stmt :: new_for :: rest in
        let goal' = Goal.replace goal ~left_stmts in
        Some goal'
      else None
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_local_rule ~apply_left side in
  LocalRule.mk_rule ~name apply

let licmup_l = create "licmup_l" Side.Left

let licmup_r = create "licmup_r" Side.Right
