open Core
open Ast.Ecoeus

let do_elim v e body rest (goal: Goal.t) =
  let f v' = if Identifier.equal v v' then Some e else None in
  let body' = Stmts.map_expr body ~f:(Expr.subst ~f) in
  let left_stmts = List.append body' rest in
  let goal' = Goal.replace goal ~left_stmts in
  Some goal'

let apply_left _ _ (goal: Goal.t) =
  match goal.left_stmts with
  | Stmt.({ bare_stmt=
              For
                { counter
                ; body=
                    [{bare_stmt= If {cond; then_branch; else_branch= []}; _}]; _
                } })
    :: left_stmts
    when not
           (Hash_set.mem
              (Stmts.write_var_set_of then_branch)
              VarBinding.{name= counter; ty= Type.IntType}) -> (
    match cond with
    | Expr.({ bare_expr=
                BinaryExpr
                  ( BinaryOperator.Eq
                  , {bare_expr= VarExpr VarBinding.({name; _}); _}
                  , e ); _ })
      when Identifier.equal name counter ->
        do_elim counter e then_branch left_stmts goal
    | Expr.({ bare_expr=
                BinaryExpr
                  ( BinaryOperator.Eq
                  , e
                  , {bare_expr= VarExpr VarBinding.({name; _}); _} ); _ })
      when Identifier.equal name counter ->
        do_elim counter e then_branch left_stmts goal
    | _ -> None )
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_local_rule ~apply_left side in
  LocalRule.mk_rule ~name apply

let loopelim1_l = create "loopelim1_l" Side.Left

let loopelim1_r = create "loopelim1_r" Side.Right
