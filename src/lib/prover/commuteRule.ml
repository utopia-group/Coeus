open Core
open Ast.Ecoeus

let find_stmts size0 size1 stmts =
  let rec find_stmts_impl acc size = function
    | _ as rest when size = 0 -> Some (List.rev acc, rest)
    | [] -> None
    | s :: rest ->
        let acc = s :: acc in
        let size = size - 1 in
        find_stmts_impl acc size rest
  in
  let open Option in
  find_stmts_impl [] size0 stmts
  >>= fun (stmts0, rest) ->
  find_stmts_impl [] size1 rest
  >>= fun (stmts1, rest) -> Some (stmts0, stmts1, rest)

let has_dependency stmts0 stmts1 =
  let is_disjoint s0 s1 =
    let s = Hash_set.inter s0 s1 in
    Hash_set.is_empty s
  in
  let is_intersection_singleton s0 s1 =
    let s = Hash_set.inter s0 s1 in
    Hash_set.length s = 1
  in
  let l0r = Stmts.read_var_set_of ~exclude_counter:true stmts0 in
  let l1r = Stmts.read_var_set_of ~exclude_counter:true stmts1 in
  let l0w = Stmts.write_var_set_of ~exclude_counter:false stmts0 in
  let l1w = Stmts.write_var_set_of ~exclude_counter:false stmts1 in
  let l0w' = Stmts.write_var_set_of ~exclude_counter:true stmts0 in
  let l1w' = Stmts.write_var_set_of ~exclude_counter:true stmts1 in
  let dep_ok s0 s0' s1 s1' =
    is_disjoint s0 s1
    || (is_intersection_singleton s0 s1 && is_disjoint s0' s1')
  in
  not
    ( dep_ok l0w l0w' l1r l1r && dep_ok l1w l1w' l0r l0r
    && dep_ok l0w l0w l1w l1w' )

let apply_left skip_size size0 size1 _ _ (goal: Goal.t) =
  if skip_size < 0 || size0 <= 0 || size1 <= 0 then None
  else
    let open Option in
    let skipped_stmts, stmts = List.split_n goal.left_stmts skip_size in
    if List.length stmts <= 1 then None
    else
      find_stmts size0 size1 stmts
      >>= fun (stmts0, stmts1, rest) ->
      if has_dependency stmts0 stmts1 then None
      else
        let left_stmts = List.concat [skipped_stmts; stmts1; stmts0; rest] in
        let goal' = Goal.replace goal ~left_stmts in
        Some goal'

let create name side skip_size size0 size1 =
  let apply =
    RuleHelper.mk_symmetric_local_rule
      ~apply_left:(apply_left skip_size size0 size1)
      side
  in
  LocalRule.mk_rule ~name apply

let commute_l = create "commute_l" Side.Left 0 1 1

let commute_r = create "commute_r" Side.Right 0 1 1

let commute1_l = create "commute1_l" Side.Left 1 1 1

let commute1_r = create "commute1_r" Side.Right 1 1 1
