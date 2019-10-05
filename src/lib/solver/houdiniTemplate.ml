open Core
open Ast.Ecoeus
open Verifier

let default_max_guesses = 1000

let seq_join hs = Sequence.interleave (Sequence.of_list hs)

let rec seq_comb k elems =
  match k with
  | 0 -> Sequence.singleton []
  | _ ->
    match elems with
    | [] -> Sequence.empty
    | x :: xs ->
        let l0 = Sequence.map ~f:(fun l -> x :: l) (seq_comb (k - 1) xs) in
        let l1 = seq_comb k xs in
        Sequence.interleave (Sequence.of_list [l0; l1])

let equal_heuristic left_vars right_vars =
  let left_seq = Sequence.of_list left_vars in
  let right_seq = Sequence.of_list right_vars in
  let cart_seq = Sequence.interleaved_cartesian_product left_seq right_seq in
  Sequence.filter_map cart_seq ~f:(fun (left_var, right_var) ->
      if [%compare.equal : VarBinding.t] left_var right_var then None
      else if not ([%compare.equal : Type.t] left_var.ty right_var.ty) then
        None
      else
        let expr =
          Expr.mk_binary BinaryOperator.Eq (Expr.mk_var left_var)
            (Expr.mk_var right_var)
        in
        Some expr )

let plus_heuristic left_vars right_vars =
  let filter_vars =
    List.filter ~f:(fun vb ->
        [%compare.equal : Type.t] vb.VarBinding.ty Type.IntType )
  in
  let left_pair_seq = seq_comb 2 (filter_vars left_vars) in
  let right_pair_seq = seq_comb 2 (filter_vars right_vars) in
  let cart_seq =
    Sequence.interleaved_cartesian_product left_pair_seq right_pair_seq
  in
  let mk_eq a b c d =
    Expr.mk_binary BinaryOperator.Eq
      (Expr.mk_binary BinaryOperator.Plus a b)
      (Expr.mk_binary BinaryOperator.Plus c d)
  in
  Sequence.concat_map cart_seq ~f:(fun (left_pair, right_pair) ->
      match (left_pair, right_pair) with
      | [left_var0; left_var1], [right_var0; right_var1]
        when not
               ( [%compare.equal : VarBinding.t] left_var0 left_var1
               && [%compare.equal : VarBinding.t] right_var0 right_var1 ) ->
          let l0 = Expr.mk_var left_var0 in
          let l1 = Expr.mk_var left_var1 in
          let r0 = Expr.mk_var right_var0 in
          let r1 = Expr.mk_var right_var1 in
          Sequence.of_list
            [mk_eq l0 l1 r0 r1; mk_eq l0 r0 l1 r1; mk_eq l0 r1 l1 r0]
      | _ -> Sequence.empty )

let cond_heuristic stmts =
  let cond_seq = Sequence.of_list (Stmts.conds_of stmts) in
  let neg_cond_seq = Sequence.map cond_seq ~f:Expr.logical_negate_of in
  seq_join [cond_seq; neg_cond_seq]

let cmp_literal_heuristic vars lits =
  let var_seq = Sequence.of_list vars in
  let lit_seq = Sequence.of_list lits in
  let cart_seq = Sequence.interleaved_cartesian_product var_seq lit_seq in
  Sequence.concat_map cart_seq ~f:(fun (v, l) ->
      match (v.VarBinding.ty, Literal.type_of l) with
      | Type.IntType, Type.IntType ->
          let ops =
            [ BinaryOperator.Lt
            ; BinaryOperator.Le
            ; BinaryOperator.Gt
            ; BinaryOperator.Ge ]
          in
          let exprs =
            List.map ops ~f:(fun bop ->
                Expr.mk_binary bop (Expr.mk_var v) (Expr.mk_literal l) )
          in
          Sequence.of_list exprs
      | _ -> Sequence.empty )

let assign_rhs_heuristics vars exprs =
  let var_seq = Sequence.of_list vars in
  let expr_seq = Sequence.of_list exprs in
  let cart_seq = Sequence.interleaved_cartesian_product var_seq expr_seq in
  Sequence.filter_map cart_seq ~f:(fun (v, e) ->
      if [%compare.equal : Type.t] v.VarBinding.ty (Expr.type_of e) then
        Some (Expr.mk_binary BinaryOperator.Eq (Expr.mk_var v) e)
      else None )

let find_stmts_vars stmts =
  let var_set = Stmts.read_write_var_set_of ~exclude_counter:false stmts in
  Hash_set.to_list var_set

let find_stmts_lits stmts =
  let conds = Stmts.conds_of stmts in
  Expr.literals_of_exprs conds

let find_stmts_assign_rhs vars stmts =
  let candidates = Stmts.assign_rhs_of stmts in
  let var_set = VarBinding.Hash_set.of_list vars in
  List.filter candidates ~f:(fun e ->
      let free_vars = Expr.free_vars_of e in
      List.for_all free_vars ~f:(Hash_set.mem var_set) )

let do_guess source =
  let open PredEnv.Source in
  match source with
  | RelLoopInv (left_stmt, right_stmt) ->
      let left_vars = find_stmts_vars [left_stmt] in
      let right_vars = find_stmts_vars [right_stmt] in
      let lits = find_stmts_lits [left_stmt; right_stmt] in
      let left_assigns = Stmt.assign_rhs_of left_stmt in
      let right_assigns = Stmt.assign_rhs_of right_stmt in
      let h0 = equal_heuristic left_vars right_vars in
      let h1 = plus_heuristic left_vars right_vars in
      let h2 = cmp_literal_heuristic (List.append left_vars right_vars) lits in
      let h3 = assign_rhs_heuristics left_vars right_assigns in
      let h4 = assign_rhs_heuristics right_vars left_assigns in
      seq_join [h0; h1; h2; h3; h4]
  | RelProcSummary (sty, left_proc, right_proc) ->
      let left_vars, right_vars =
        let open PredEnv.SummaryType in
        match sty with
        | Precondition -> (left_proc.params, right_proc.params)
        | Postcondition ->
            ( List.append left_proc.rets left_proc.params
            , List.append right_proc.rets right_proc.params )
      in
      let lits =
        find_stmts_lits (List.append left_proc.stmts right_proc.stmts)
      in
      let left_assigns = find_stmts_assign_rhs left_vars left_proc.stmts in
      let right_assigns = find_stmts_assign_rhs right_vars right_proc.stmts in
      let h0 = equal_heuristic left_vars right_vars in
      let h1 = plus_heuristic left_vars right_vars in
      let h2 = cmp_literal_heuristic (List.append left_vars right_vars) lits in
      let h3 = assign_rhs_heuristics left_vars right_assigns in
      let h4 = assign_rhs_heuristics right_vars left_assigns in
      seq_join [h0; h1; h2; h3; h4]
  | LoopInv stmt -> cond_heuristic [stmt]
  | Unknown | ProcSummary _ ->
      (* We don't have any intelligent guess strategies in these situations *)
      Sequence.empty

let guess ?(max_guesses= default_max_guesses) source =
  let seq = do_guess source in
  Sequence.(to_list (take seq max_guesses))
