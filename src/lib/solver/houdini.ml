open Core
open Ast.Ecoeus
module SolutionTable = Identifier.Table

module DepGraph = struct
  include Graph.Imperative.Digraph.Concrete (Identifier)

  (* We fabricate a special node "false" that represents the false head *)
  let false_node = Identifier.of_string "false"

  (* We fabricate a special node "root" that points to every other node (used as the entry point for WeakTopo) *)
  let root_node = Identifier.of_string "root"

  let of_horn_ast ast =
    let graph = create ~size:(HornAst.num_preds ast) () in
    add_edge graph root_node false_node ;
    let preds = HornAst.preds_of ast in
    List.iter preds ~f:(fun pred ->
        add_edge graph root_node pred.HornAst.Predicate.name) ;
    let rules = HornAst.rules_of ast in
    let process_rule (rule : HornAst.Rule.t) =
      let head_name =
        let open HornAst.Rule.Head in
        match rule.head with
        | False ->
            false_node
        | Predicate {name; _} ->
            name
      in
      let preds = HornAst.Rule.Bodies.predicates_of rule.bodies in
      List.iter preds ~f:(fun pred -> add_edge graph pred head_name)
    in
    List.iter rules ~f:process_rule ;
    graph
end

module WeakTopo = Graph.WeakTopological.Make (DepGraph)

let init_solution table preds =
  List.iter preds ~f:(fun (pred : HornAst.Predicate.t) ->
      let pred_name = pred.name in
      let pred_candidates = pred.hints in
      SolutionTable.set table ~key:pred_name ~data:pred_candidates)

let update_solution table pred keep_set =
  SolutionTable.update table pred ~f:(function
    | None ->
        let msg =
          Fmt.strf "[INTERNAL] Cannot find solution for predicate %a"
            Identifier.pp pred
        in
        failwith msg
    | Some candidates ->
        let new_candidates =
          if Hash_set.is_empty keep_set then []
          else
            List.filteri candidates ~f:(fun idx _ -> Hash_set.mem keep_set idx)
        in
        new_candidates)

module InternalStatus = struct
  type t = Verified | Rejected | Undeterminted

  let merge s0 s1 =
    match (s0, s1) with
    | Rejected, _ | _, Rejected ->
        Rejected
    | Verified, Verified ->
        Verified
    | _ ->
        Undeterminted
end

module Discharger = struct
  module Status = struct
    type t = Verified | Rejected | Pruned of Int.Hash_set.t
  end

  let discharge (z3env : Z3Env.t) pre_conds opt_post_conds =
    (* We want to check the validity of "conjunct(pres) ==> conjunct(posts)" *)
    let z3_pres = List.map pre_conds ~f:(Z3Encoder.encode_expr z3env) in
    let z3_pre = Z3.Boolean.mk_and z3env.ctx z3_pres in
    let z3_posts =
      match opt_post_conds with
      | None ->
          [Z3.Boolean.mk_false z3env.ctx]
      | Some post_conds ->
          List.map post_conds ~f:(Z3Encoder.encode_expr z3env)
    in
    let z3_query =
      let open Z3.Boolean in
      let z3_post = mk_and z3env.ctx z3_posts in
      mk_not z3env.ctx (mk_implies z3env.ctx z3_pre z3_post)
    in
    let open Z3.Solver in
    let solver = Z3Env.mk_solver z3env in
    add solver [z3_query] ;
    match check solver [] with
    | UNSATISFIABLE ->
        Status.Verified
    | UNKNOWN ->
        Status.Rejected
    | SATISFIABLE -> (
      match opt_post_conds with
      | None ->
          Status.Rejected
      | Some post_conds -> (
        match get_model solver with
        | None ->
            Status.Rejected
        | Some model ->
            let keep_set =
              Int.Hash_set.create ~size:(List.length post_conds) ()
            in
            List.iteri z3_posts ~f:(fun idx z3_post ->
                let query = Z3.Boolean.mk_implies z3env.ctx z3_pre z3_post in
                (* Set model_completion to true and hope we get a concrete boolean value *)
                match Z3.Model.eval model query true with
                | Some e when Z3.Boolean.is_true e ->
                    Hash_set.add keep_set idx
                | _ ->
                    (* Be conservative and filter out this expression *)
                    ()) ;
            Status.Pruned keep_set ) )
end

exception SolutionRejected

let run_houdini z3env ast =
  let solution_table = SolutionTable.create ~size:(HornAst.num_preds ast) () in
  init_solution solution_table (HornAst.preds_of ast) ;
  let dep_graph = DepGraph.of_horn_ast ast in
  let weak_topo = WeakTopo.recursive_scc dep_graph DepGraph.root_node in
  let predicate_lookup name args =
    let exprs = SolutionTable.find_exn solution_table name in
    let pred = HornAst.lookup_pred_exn ast name in
    let subst_map =
      Identifier.Map.of_alist_exn
        (List.map (List.zip_exn pred.params args) ~f:(fun (param, arg) ->
             (param.VarBinding.name, arg)))
    in
    List.map exprs ~f:(Expr.subst ~f:(Identifier.Map.find subst_map))
  in
  let predicate_subst name args =
    Expr.conjunct_exprs (predicate_lookup name args)
  in
  let process_pred pred =
    match pred with
    | p when p = DepGraph.root_node ->
        (* Root node trivially resolves *)
        InternalStatus.Verified
    | _ ->
        let rules = HornAst.rules_of_head ast pred in
        List.fold rules ~init:InternalStatus.Verified ~f:(fun acc rule ->
            let open InternalStatus in
            match acc with
            | Rejected ->
                acc
            | _ ->
                let pre_conds =
                  HornAst.Rule.Bodies.subst_predicate ~f:predicate_subst
                    rule.bodies
                in
                let opt_post_conds =
                  let open HornAst.Rule.Head in
                  match rule.head with
                  | False ->
                      None
                  | Predicate {name; args} ->
                      let conds = predicate_lookup name args in
                      Some conds
                in
                let rule_status =
                  let open Discharger in
                  match discharge z3env pre_conds opt_post_conds with
                  | Status.Verified ->
                      InternalStatus.Verified
                  | Status.Rejected ->
                      InternalStatus.Rejected
                  | Status.Pruned keep_set ->
                      update_solution solution_table pred keep_set ;
                      InternalStatus.Undeterminted
                in
                merge acc rule_status)
  in
  let rec iterate_components components =
    Graph.WeakTopological.fold_left iterate_component () components
  and iterate_component _ component =
    let open Graph.WeakTopological in
    match component with
    | Component (leader, components) ->
        stablize false leader components
    | Vertex v -> (
      match process_pred v with
      | InternalStatus.Rejected ->
          raise SolutionRejected
      | _ ->
          () )
  and stablize can_stop leader components =
    let solution_changed =
      match process_pred leader with
      | InternalStatus.Rejected ->
          raise SolutionRejected
      | InternalStatus.Verified ->
          false
      | InternalStatus.Undeterminted ->
          true
    in
    if can_stop && not solution_changed then ()
    else (
      iterate_components components ;
      stablize true leader components )
  in
  try
    iterate_components weak_topo ;
    SolutionTable.iteri solution_table ~f:(fun ~key ~data ->
        let pp_and fmt () = Fmt.string fmt " && " in
        Logs.info (fun m ->
            m "Final solution for %a: %a" Identifier.pp key
              (Fmt.list ~sep:pp_and Expr.pp)
              data)) ;
    Status.Verified
  with SolutionRejected ->
    (* Houdini verifier can never reject vstate because of its incomplete nature *)
    Status.Unknown
