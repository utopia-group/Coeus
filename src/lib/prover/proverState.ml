open Core
open Ast.Ecoeus
open Verifier

type t =
  { ast: Ast.Ecoeus.t
  ; depth: int
  ; goals: Goal.t list
  ; verif_state: Verifier.VerifState.t }

let init ast =
  let EntrySpec.({left; right}) = ast.entry in
  let Spec.({requires; ensures}) = ast.spec in
  let left_proc : Procedure.t = lookup_proc_exn ast left in
  let right_proc : Procedure.t = lookup_proc_exn ast right in
  let free_bindings =
    List.concat
      [ left_proc.params
      ; left_proc.rets
      ; left_proc.locals
      ; right_proc.params
      ; right_proc.rets
      ; right_proc.locals ]
  in
  (* This is important since it ensures assignments are correctly handled *)
  let havoc_cond = Precondition.mk_havoc free_bindings in
  let assumption = Precondition.mk_assume requires in
  let pre_conds = [assumption; havoc_cond] in
  let post_cond = Postcondition.mk_assert ~is_final:true ensures in
  let goal =
    Goal.mk ~subgoal_index:0 pre_conds post_cond left_proc.name left_proc.stmts
      right_proc.name right_proc.stmts
  in
  let verif_state = VerifState.empty in
  ProverState.{ast; depth= 0; goals= [goal]; verif_state}

let pp fmt {goals; verif_state; _} =
  Fmt.pf fmt "ProverState([| %a |]; %a)"
    (Fmt.list ~sep:Fmt.comma Goal.pp)
    goals VerifState.pp verif_state

let num_goals {goals; _} = List.length goals

let stmt_count {goals; _} =
  List.fold goals ~init:0 ~f:(fun acc goal -> acc + Goal.stmt_count goal)

let ast_size_of {goals; verif_state; _} =
  List.fold goals ~init:0 ~f:(fun acc goal -> acc + Goal.ast_size_of goal)
  + VerifState.ast_size_of verif_state

let current_goal_of {goals; _} =
  match goals with [] -> None | goal :: _ -> Some goal

let is_empty {goals; _} = List.for_all goals ~f:Goal.is_empty

let is_fully_resolved {goals; _} = List.is_empty goals

let rotate_top state =
  match state.goals with
  | [] -> state
  | goal :: rest ->
      let new_goals = List.append rest [goal] in
      {state with goals= new_goals}

let rec resolve_top state =
  match state.goals with
  | goal :: rest when Goal.is_empty goal ->
      let new_vc = Goal.extract_vc goal in
      let verif_state = VerifState.add_vc state.verif_state new_vc in
      let state' = {state with goals= rest; verif_state} in
      resolve_top state'
  | _ -> state

let resolve state =
  let add_goal goal verif_state =
    let new_vc = Goal.extract_vc goal in
    VerifState.add_vc verif_state new_vc
  in
  let goals, verif_state =
    List.fold (List.rev state.goals) ~init:([], state.verif_state)
      ~f:(fun (acc, verif_state) goal ->
        if Goal.is_empty goal then
          let verif_state = add_goal goal verif_state in
          (acc, verif_state)
        else
          let acc = goal :: acc in
          (acc, verif_state) )
  in
  {state with goals; verif_state}

let assign_subgoal_indices state =
  let rec impl idx acc = function
    | [] -> List.rev acc
    | goal :: rest -> (
      match goal.Goal.subgoal_index with
      | Some _ -> List.append (List.rev acc) (goal :: rest)
      | None ->
          let goal' = Goal.set_subgoal_index idx goal in
          let acc' = goal' :: acc in
          impl (idx + 1) acc' rest )
  in
  let goals = impl 0 [] state.goals in
  assert (List.length goals = List.length state.goals) ;
  {state with goals}

let current_subgoal_index_of state =
  match state.goals with [] -> None | goal :: _ -> goal.Goal.subgoal_index

let check_depth_limit ProverConfig.({depth_limit; _}) {depth; _} =
  match depth_limit with
  | Some limit when depth > limit ->
      let msg = "Depth limit exceeded" in
      Result.Error msg
  | _ -> Result.Ok ()

let check_goal_limit ProverConfig.({goal_limit; _}) {goals; _} =
  match goal_limit with
  | None -> Result.Ok ()
  | Some limit ->
      let num_goals = List.length goals in
      if num_goals > limit then
        let msg = "Goal limit exceeded" in
        Result.Error msg
      else Result.Ok ()

let check_ast_size_limit ProverConfig.({ast_size_limit; _}) state =
  match ast_size_limit with
  | None -> Result.Ok ()
  | Some limit ->
      let ast_size = ast_size_of state in
      if ast_size > limit then
        let msg = "AST size limit exceeded" in
        Result.Error msg
      else Result.Ok ()

let check_memory_limit ProverConfig.({memory_limit; _}) _ =
  match memory_limit with
  | None -> Result.Ok ()
  | Some limit ->
      let mem_stat = Gc.stat () in
      let live_bytes = mem_stat.live_words * Sys.word_size in
      if live_bytes > limit then
        let msg = "Memory usage limit exceeded" in
        Result.Error msg
      else Result.Ok ()

let check_limit config state =
  let open Result in
  check_depth_limit config state
  >>= fun _ ->
  check_goal_limit config state
  >>= fun _ ->
  check_ast_size_limit config state
  >>= fun _ -> check_memory_limit config state

let exceeds_limit config state = Result.is_error (check_limit config state)
