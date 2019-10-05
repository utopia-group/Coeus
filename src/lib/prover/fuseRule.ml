open Core
open Ast.Ecoeus

let can_fuse stmt0 stmt1 =
  let is_disjoint s0 s1 =
    let s = Hash_set.inter s0 s1 in
    Hash_set.is_empty s
  in
  let is_intersection_singleton s0 s1 =
    let s = Hash_set.inter s0 s1 in
    Hash_set.length s = 1
  in
  let l0r = Stmt.read_var_set_of ~exclude_counter:true stmt0 in
  let l1r = Stmt.read_var_set_of ~exclude_counter:true stmt1 in
  let l0w = Stmt.write_var_set_of ~exclude_counter:false stmt0 in
  let l1w = Stmt.write_var_set_of ~exclude_counter:false stmt1 in
  let l0w' = Stmt.write_var_set_of ~exclude_counter:true stmt0 in
  let l1w' = Stmt.write_var_set_of ~exclude_counter:true stmt1 in
  let dep_ok s0 s0' s1 s1' =
    is_disjoint s0 s1
    || (is_intersection_singleton s0 s1 && is_disjoint s0' s1')
  in
  dep_ok l0w l0w' l1r l1r && dep_ok l1w l1w' l0r l0r && dep_ok l0w l0w l1w l1w'

let fuse_for_loops parent counter0 counter1 lower upper step direction body0
    body1 rest_stmts =
  let body1', leftover_stmts =
    if Identifier.equal counter0 counter1 then (body1, [])
    else
      let final_value =
        Expr.mk_var VarBinding.{name= counter0; ty= Type.IntType}
      in
      let stmt =
        Stmt.mk_assign parent
          (Lvalue.mk_var VarBinding.{name= counter1; ty= Type.IntType})
          final_value
      in
      ( Stmts.map_var body1 ~f:(fun v ->
            if Identifier.equal v counter1 then counter0 else v )
      , [stmt] )
  in
  let body' = List.append body0 body1' in
  let fused_loop =
    Stmt.mk_for parent counter0 lower upper step direction body'
  in
  List.append (fused_loop :: leftover_stmts) rest_stmts

let apply_left _ _ (goal: Goal.t) =
  match goal.left_stmts with
  | ( Stmt.({ bare_stmt=
                For
                  { counter= counter0
                  ; lower= lower0
                  ; upper= upper0
                  ; step= step0
                  ; direction= direction0
                  ; body= body0 }; _ }) as s0 )
    :: ( Stmt.({ bare_stmt=
                   For
                     { counter= counter1
                     ; lower= lower1
                     ; upper= upper1
                     ; step= step1
                     ; direction= direction1
                     ; body= body1 }; _ }) as s1 )
       :: left_stmts
    when Expr.equal (Expr.simplify lower0) (Expr.simplify lower1)
         && Expr.equal (Expr.simplify upper0) (Expr.simplify upper1)
         && Expr.equal (Expr.simplify step0) (Expr.simplify step1)
         && Stmt.ForDirection.equal direction0 direction1
         && can_fuse s0 s1 ->
      let left_stmts =
        fuse_for_loops goal.left_proc counter0 counter1 lower0 upper0 step0
          direction0 body0 body1 left_stmts
      in
      let goal' = Goal.replace goal ~left_stmts in
      Some goal'
  | _ -> None

let create name side =
  let apply = RuleHelper.mk_symmetric_local_rule ~apply_left side in
  LocalRule.mk_rule ~name apply

let fuse_l = create "fuse_l" Side.Left

let fuse_r = create "fuse_r" Side.Right
