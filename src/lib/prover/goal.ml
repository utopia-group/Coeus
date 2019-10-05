open Core
open Ast.Ecoeus
open Verifier

type t =
  { pre_conds: Verifier.Precondition.t list
  ; post_cond: Verifier.Postcondition.t
  ; left_proc: Ast.Ecoeus.Identifier.t
  ; left_stmts: Ast.Ecoeus.Stmt.t list
  ; right_proc: Ast.Ecoeus.Identifier.t
  ; right_stmts: Ast.Ecoeus.Stmt.t list
  ; blame_step: int option
  ; prev_step: int option
  ; subgoal_index: int option
  ; ast_size: int }

let mk ?blame_step ?prev_step ?subgoal_index pre_conds post_cond left_proc
    left_stmts right_proc right_stmts =
  let ast_size =
    3
    + List.fold pre_conds ~init:0 ~f:(fun acc p ->
          acc + Precondition.ast_size_of p )
    + Postcondition.ast_size_of post_cond
    + Stmts.ast_size_of left_stmts
    + Stmts.ast_size_of right_stmts
  in
  { pre_conds
  ; post_cond
  ; left_proc
  ; left_stmts
  ; right_proc
  ; right_stmts
  ; blame_step
  ; prev_step
  ; subgoal_index
  ; ast_size }

let replace ?pre_conds ?post_cond ?left_proc ?left_stmts ?right_proc
    ?right_stmts goal =
  let blame_step = goal.blame_step in
  let prev_step = goal.prev_step in
  let pre_conds = Option.value pre_conds ~default:goal.pre_conds in
  let post_cond = Option.value post_cond ~default:goal.post_cond in
  let left_proc = Option.value left_proc ~default:goal.left_proc in
  let left_stmts = Option.value left_stmts ~default:goal.left_stmts in
  let right_proc = Option.value right_proc ~default:goal.right_proc in
  let right_stmts = Option.value right_stmts ~default:goal.right_stmts in
  mk ?blame_step ?prev_step pre_conds post_cond left_proc left_stmts right_proc
    right_stmts

let assign_blame blame_step goal = {goal with blame_step}

let set_prev prev_step goal = {goal with prev_step}

let set_subgoal_index i goal = {goal with subgoal_index= Some i}

let ast_size_of {ast_size; _} = ast_size

let pp fmt {pre_conds; post_cond; left_stmts; right_stmts; subgoal_index; _} =
  Fmt.pf fmt
    "Goal(pre = [ %a ]; left = %a; right = %a; post = %a; subgoal_index = %a)"
    (Fmt.list ~sep:Fmt.comma Precondition.pp)
    pre_conds Stmts.pp_brief left_stmts Stmts.pp_brief right_stmts
    Postcondition.pp post_cond (Fmt.option Fmt.int) subgoal_index

let stmt_count {left_stmts; right_stmts; _} =
  Stmts.count left_stmts + Stmts.count right_stmts

let is_empty {left_stmts; right_stmts; _} =
  List.is_empty left_stmts && List.is_empty right_stmts

let swap_stmts goal =
  let left_stmts = goal.right_stmts in
  let right_stmts = goal.left_stmts in
  {goal with left_stmts; right_stmts}

let extract_vc {pre_conds; post_cond; blame_step; _} =
  VerifCondition.mk ?blame_step pre_conds post_cond
