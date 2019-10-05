open Core
open Ast.Ecoeus
open Prover

let tree_strategy search_config prover_state =
  match ProverState.current_goal_of prover_state with
  | None -> []
  | Some Goal.({left_stmts; right_stmts; _}) ->
      let try_rules =
        match (left_stmts, right_stmts) with
        | loop0 :: _, loop1 :: _ when Stmt.is_loop loop0 && Stmt.is_loop loop1 ->
            [SyncRule.sync_n; SeqRule.seq_l]
        | _ -> [BlastseqTactic.blastseq]
      in
      List.map try_rules (fun r ->
          match
            ProverAction.apply_rule r search_config.SearchConfig.prover_config
              prover_state
          with
          | Result.Error msg ->
              let msg =
                Fmt.strf
                  "[INTERNAL] DescartesStrategy generated a candidate rule \
                   \"%a\" that is not applicable: %s"
                  Rule.pp r msg
              in
              failwith msg
          | Result.Ok s -> (r, s) )

let create () = TreeStrategy.dfs tree_strategy
