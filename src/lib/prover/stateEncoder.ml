open Core
open Ast.Ecoeus

let rec expr_structural_same (e0 : Expr.t) (e1 : Expr.t) =
  match (e0.bare_expr, e1.bare_expr) with
  | Expr.LiteralExpr l0, Expr.LiteralExpr l1 ->
      [%compare.equal: Literal.t] l0 l1
  | Expr.VarExpr _, Expr.VarExpr _ -> true
  | Expr.UnaryExpr (op0, e0), Expr.UnaryExpr (op1, e1) ->
      [%compare.equal: UnaryOperator.t] op0 op1 && expr_structural_same e0 e1
  | Expr.BinaryExpr (op0, lhs0, rhs0), Expr.BinaryExpr (op1, lhs1, rhs1) ->
      [%compare.equal: BinaryOperator.t] op0 op1
      && expr_structural_same lhs0 lhs1
      && expr_structural_same rhs0 rhs1
  | ( Expr.ArraySelectExpr {base= b0; indices= i0}
    , Expr.ArraySelectExpr {base= b1; indices= i1} ) ->
      expr_structural_same b0 b1 && exprs_structural_same i0 i1
  | ( Expr.ArrayStoreExpr {base= b0; indices= i0; value= v0}
    , Expr.ArrayStoreExpr {base= b1; indices= i1; value= v1} ) ->
      expr_structural_same b0 b1 && expr_structural_same v0 v1
      && exprs_structural_same i0 i1
  | ( Expr.CondExpr {cond= c0; true_val= t0; false_val= f0}
    , Expr.CondExpr {cond= c1; true_val= t1; false_val= f1} ) ->
      expr_structural_same c0 c1 && expr_structural_same t0 t1
      && expr_structural_same f0 f1
  | Expr.FunCallExpr {func= f0; args= a0}, Expr.FunCallExpr {func= f1; args= a1}
    ->
      Identifier.equal f0.name f1.name && exprs_structural_same a0 a1
  | ( Expr.QuantifiedExpr {quantifier= q0; body= b0; _}
    , Expr.QuantifiedExpr {quantifier= q1; body= b1; _} ) ->
      [%compare.equal: Quantifier.t] q0 q1 && expr_structural_same b0 b1
  | _ -> false

and exprs_structural_same es0 es1 =
  match List.zip es0 es1 with
  | None -> false
  | Some eps -> List.for_all eps ~f:(fun (e0, e1) -> expr_structural_same e0 e1)

let lval_structural_same (l0 : Lvalue.t) (l1 : Lvalue.t) =
  [%compare.equal: VarBinding.t] l0.base l1.base
  && exprs_structural_same l0.indices l1.indices

let stmt_root_structural_same (s0 : Stmt.t) (s1 : Stmt.t) =
  match (s0.bare_stmt, s1.bare_stmt) with
  | Stmt.Assign {lhs= l0; rhs= r0}, Stmt.Assign {lhs= l1; rhs= r1} ->
      lval_structural_same l0 l1 && expr_structural_same r0 r1
  | Stmt.Assume e0, Stmt.Assume e1 -> expr_structural_same e0 e1
  | Stmt.If {cond= c0; _}, Stmt.If {cond= c1; _} -> expr_structural_same c0 c1
  | Stmt.While {cond= c0; _}, Stmt.While {cond= c1; _} ->
      expr_structural_same c0 c1
  | ( Stmt.For {lower= l0; upper= u0; step= s0; direction= d0; _}
    , Stmt.For {lower= l1; upper= u1; step= s1; direction= d1; _} ) ->
      [%compare.equal: Stmt.ForDirection.t] d0 d1
      && expr_structural_same l0 l1 && expr_structural_same u0 u1
      && expr_structural_same s0 s1
  | _ -> false

let rec estimate_stmt_difficulty_impl stmts =
  let open Stmt in
  match stmts with
  | [] -> 0
  | Stmt.({bare_stmt= Assume _ | Assign _; _}) :: rest ->
      estimate_stmt_difficulty_impl rest
  | Stmt.({bare_stmt= Call _; _}) :: rest ->
      3 + estimate_stmt_difficulty_impl rest
  | Stmt.({bare_stmt= If {then_branch; else_branch; _}; _}) :: rest ->
      let then_d = estimate_stmt_difficulty_impl then_branch in
      let else_d = estimate_stmt_difficulty_impl else_branch in
      let rest_d = estimate_stmt_difficulty_impl rest in
      let if_d = if then_d > 0 || else_d > 0 then 1 else 0 in
      if_d + then_d + else_d + rest_d
  | Stmt.({bare_stmt= While {body; _} | For {body; _}; _}) :: rest ->
      let body_d = estimate_stmt_difficulty_impl body in
      let rest_d = estimate_stmt_difficulty_impl rest in
      2 + body_d + rest_d

let estimate_stmt_difficulty = function
  | None -> (None, None)
  | Some (goal : Goal.t) ->
      let left_d = 1 + estimate_stmt_difficulty_impl goal.left_stmts in
      let right_d = 1 + estimate_stmt_difficulty_impl goal.right_stmts in
      (Some left_d, Some right_d)

let get_stmt_type stmt =
  let open Stmt in
  match stmt.bare_stmt with
  | Assume _ -> 1
  | Assign _ -> 2
  | If _ -> 3
  | While _ -> 4
  | For _ -> 5
  | Call _ -> 6

let stmt_type_from_goal idx = function
  | None -> (None, None)
  | Some (goal : Goal.t) ->
      let open Option in
      let left_t = List.nth goal.left_stmts idx >>| get_stmt_type in
      let right_t = List.nth goal.right_stmts idx >>| get_stmt_type in
      (left_t, right_t)

let get_ratio expr0 expr1 =
  let open Expr in
  match (expr0.bare_expr, expr1.bare_expr) with
  | _, _ when Expr.equal expr0 expr1 -> Some 0.0
  | LiteralExpr (Literal.IntLit i), LiteralExpr (Literal.IntLit j)
    when (not Bigint.(equal i zero)) && not Bigint.(equal j zero) ->
      let ratio = Bigint.to_float i /. Bigint.to_float j in
      let log2ratio = log ratio /. log 2.0 in
      if Float.is_inf log2ratio || Float.is_nan log2ratio then None
      else Some log2ratio
  | _ -> None

let check_for_stmt = function
  | None -> (None, None, None)
  | Some (goal : Goal.t) -> (
    match (goal.left_stmts, goal.right_stmts) with
    | ( Stmt.({ bare_stmt=
                  For
                    { lower= lower0
                    ; upper= upper0
                    ; step= step0
                    ; direction= direction0; _ }; _ })
        :: _
      , Stmt.({ bare_stmt=
                  For
                    { lower= lower1
                    ; upper= upper1
                    ; step= step1
                    ; direction= direction1; _ }; _ })
        :: _ )
      when Stmt.ForDirection.equal direction0 direction1 ->
        let lower_ratio = get_ratio lower0 lower1 in
        let upper_ratio = get_ratio upper0 upper1 in
        let step_ratio = get_ratio step0 step1 in
        (lower_ratio, upper_ratio, step_ratio)
    | _ -> (None, None, None) )

let check_for_hints = function
  | None -> (None, None, None, None)
  | Some (goal : Goal.t) -> (
    match (goal.left_stmts, goal.right_stmts) with
    | ( Stmt.({bare_stmt= For {lower= lower0; upper= upper0; _}; _}) :: _
      , Stmt.({bare_stmt= For {lower= lower1; upper= upper1; _}; _}) :: _ ) ->
        let is_literal Expr.({bare_expr; _}) =
          match bare_expr with Expr.LiteralExpr _ -> true | _ -> false
        in
        let l0 = is_literal lower0 in
        let l1 = is_literal lower1 in
        let u0 = is_literal upper0 in
        let u1 = is_literal upper1 in
        let forward_lhint = Bool.equal l0 l1 in
        let forward_uhint = Bool.equal u0 u1 in
        let backward_lhint = Bool.equal l0 u1 in
        let backward_uhint = Bool.equal l1 u0 in
        ( Some forward_lhint
        , Some forward_uhint
        , Some backward_lhint
        , Some backward_uhint )
    | _ -> (None, None, None, None) )

let get_loop_count stmt =
  let open Stmt in
  match stmt.bare_stmt with
  | For
      { lower= Expr.({bare_expr= LiteralExpr (Literal.IntLit l); _})
      ; upper= Expr.({bare_expr= LiteralExpr (Literal.IntLit u); _}); _ } ->
      let diff = Bigint.(u - l) in
      Bigint.to_int diff
  | _ -> None

let check_loop_counts = function
  | None -> (None, None)
  | Some (goal : Goal.t) ->
      let open Option in
      let left_c = List.hd goal.left_stmts >>= get_loop_count in
      let right_c = List.hd goal.right_stmts >>= get_loop_count in
      (left_c, right_c)

let has_complex_cond stmt =
  let open Stmt in
  match stmt.bare_stmt with
  | If {cond; _} | While {cond; _} -> (
      let open Expr in
      match cond.bare_expr with
      | BinaryExpr ((BinaryOperator.And | BinaryOperator.Or), _, _) ->
          Some true
      | UnaryExpr (UnaryOperator.Not, _) -> Some true
      | _ -> Some false )
  | _ -> None

let check_while_stmt = function
  | None -> (None, None)
  | Some (goal : Goal.t) ->
      let open Option in
      let left_flag = List.hd goal.left_stmts >>= has_complex_cond in
      let right_flag = List.hd goal.right_stmts >>= has_complex_cond in
      (left_flag, right_flag)

let check_stmt_root = function
  | None -> true
  | Some (goal : Goal.t) -> (
    match (goal.left_stmts, goal.right_stmts) with
    | [], [] -> true
    | s0 :: _, s1 :: _ -> stmt_root_structural_same s0 s1
    | _ -> false )

let to_encoding prover_config (prover_state : ProverState.t) =
  let opt_goal = ProverState.current_goal_of prover_state in
  let left_stmt_difficulty, right_stmt_difficulty =
    estimate_stmt_difficulty opt_goal
  in
  let left_stmt_type, right_stmt_type = stmt_type_from_goal 0 opt_goal in
  let left_stmt1_type, right_stmt1_type = stmt_type_from_goal 1 opt_goal in
  let left_stmt2_type, right_stmt2_type = stmt_type_from_goal 2 opt_goal in
  let left_while_complex_cond, right_while_complex_cond =
    check_while_stmt opt_goal
  in
  let left_for_count, right_for_count = check_loop_counts opt_goal in
  let stmt_structural_same = check_stmt_root opt_goal in
  let for_lbound_ratio, for_ubound_ratio, for_step_ratio =
    check_for_stmt opt_goal
  in
  let for_flhint, for_fuhint, for_blhint, for_buhint =
    check_for_hints opt_goal
  in
  let mk_app rule = rule.Rule.is_applicable prover_config prover_state in
  let is_autoseq_app = mk_app AutoseqTactic.autoseq in
  let is_blastseq_app = mk_app BlastseqTactic.blastseq in
  let is_extend_app = mk_app ExtendRule.extend in
  let is_syncif_app = mk_app SyncIfRule.syncif in
  let is_peell_app = mk_app PeelRule.peel_l in
  let is_peelr_app = mk_app PeelRule.peel_r in
  let is_reversel_app = mk_app ReverseRule.reverse_l in
  let is_reverser_app = mk_app ReverseRule.reverse_r in
  let is_commutel_app = mk_app CommuteRule.commute_l in
  let is_commuter_app = mk_app CommuteRule.commute_r in
  let is_commute1l_app = mk_app CommuteRule.commute1_l in
  let is_commute1r_app = mk_app CommuteRule.commute1_r in
  let is_loopelim1l_app = mk_app LoopElimRule.loopelim1_l in
  let is_loopelim1r_app = mk_app LoopElimRule.loopelim1_r in
  let is_fusel_app = mk_app FuseRule.fuse_l in
  let is_fuser_app = mk_app FuseRule.fuse_r in
  let is_sfusel_app = mk_app StrongFuseRule.sfuse_l in
  let is_sfuser_app = mk_app StrongFuseRule.sfuse_r in
  let is_looptorecl_app = mk_app LoopToRecRule.looptorec_l in
  let is_looptorecr_app = mk_app LoopToRecRule.looptorec_r in
  let is_synccall_app = mk_app SyncCallRule.synccall in
  let is_inlinel_app = mk_app InlineRule.inline_l in
  let is_inliner_app = mk_app InlineRule.inline_r in
  let is_concatl_app = mk_app ConcatRule.concat_l in
  let is_concatr_app = mk_app ConcatRule.concat_r in
  StateEncoding.Fields.create ~curr_depth:prover_state.depth
    ~goals_remaining:(ProverState.num_goals prover_state)
    ~ast_size:(ProverState.ast_size_of prover_state)
    ~left_stmt_type ~left_stmt1_type ~left_stmt2_type ~right_stmt_type
    ~right_stmt1_type ~right_stmt2_type ~left_stmt_difficulty
    ~right_stmt_difficulty ~left_while_complex_cond ~right_while_complex_cond
    ~left_for_count ~right_for_count ~stmt_structural_same ~for_ubound_ratio
    ~for_lbound_ratio ~for_step_ratio ~for_flhint ~for_fuhint ~for_blhint
    ~for_buhint ~is_autoseq_app ~is_blastseq_app ~is_extend_app ~is_syncif_app
    ~is_peell_app ~is_peelr_app ~is_reversel_app ~is_reverser_app
    ~is_commutel_app ~is_commuter_app ~is_commute1l_app ~is_commute1r_app
    ~is_loopelim1l_app ~is_loopelim1r_app ~is_fusel_app ~is_fuser_app
    ~is_sfusel_app ~is_sfuser_app ~is_looptorecl_app ~is_looptorecr_app
    ~is_synccall_app ~is_inlinel_app ~is_inliner_app ~is_concatl_app
    ~is_concatr_app

let encode prover_config prover_state =
  let encoding = to_encoding prover_config prover_state in
  let flist = StateEncoding.to_feature_vec prover_config encoding in
  List.to_array flist
