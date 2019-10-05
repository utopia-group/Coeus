open Core
open Prover

module Trace = struct
  type t = {rule: Rule.t; subgoal_index: int option}
end

let pp_subgoal_index fmt = function
  | None -> ()
  | Some i -> Fmt.pf fmt "<%d>" i

let str_of_traces traces =
  String.concat
    (List.map traces ~f:(fun Trace.({rule; subgoal_index}) ->
         Fmt.strf "%c%a" (Rules.char_of_exn rule) pp_subgoal_index
           subgoal_index ))

module ConflictSpec = struct
  type t = Re2.t

  let spec_match traces spec =
    let s = str_of_traces traces in
    Re2.matches spec s

  let pp fmt spec = Fmt.pf fmt "ConflictSpec( %s )" (Re2.pattern spec)
end

module ConflictSpecs = struct
  type t = ConflictSpec.t list

  let specs_match prefixes specs =
    List.for_all specs ~f:(fun spec ->
        List.exists prefixes ~f:(fun prefix ->
            ConflictSpec.spec_match prefix spec ) )

  let pp fmt = Fmt.pf fmt "{ %a }" (Fmt.list ~sep:Fmt.comma ConflictSpec.pp)
end

module ConflictCache = struct
  (* A simple LRU cache implemented with doubly linked list *)
  type t = {cache: ConflictSpecs.t Doubly_linked.t; size: int}

  let create ~size =
    ( if size < 0 then
      let msg = Fmt.strf "Cache size cannot be negative: %d" size in
      raise (Invalid_argument msg) ) ;
    let cache = Doubly_linked.create () in
    {cache; size}

  let add_specs {cache; size} specs =
    let cache_size = Doubly_linked.length cache in
    if cache_size >= size then ignore (Doubly_linked.remove_last cache) ;
    ignore (Doubly_linked.insert_first cache specs)

  let lookup {cache; _} prefixes =
    match
      Doubly_linked.find_elt cache ~f:(ConflictSpecs.specs_match prefixes)
    with
    | None -> false
    | Some elt ->
        Doubly_linked.move_to_front cache elt ;
        true

  let length {cache; _} = Doubly_linked.length cache

  let max_size {size; _} = size

  let clear {cache; _} = Doubly_linked.clear cache
end

let extract_index_prefix_impl step_table blame_index =
  let rec impl acc index =
    let step : ExhaustiveSearchState.ProofStep.t = step_table.(index) in
    let acc = index :: acc in
    match step.prev_depth with None -> acc | Some index -> impl acc index
  in
  impl [] blame_index

let extract_prefix_impl step_table blame_index =
  let rec impl acc index =
    let step : ExhaustiveSearchState.ProofStep.t = step_table.(index) in
    let trace = Trace.{rule= step.rule; subgoal_index= step.subgoal_index} in
    let acc = trace :: acc in
    match step.prev_depth with None -> acc | Some index -> impl acc index
  in
  impl [] blame_index

(* let extract_prefix ExhaustiveSearchState.({rev_history; _}) =
 *   match rev_history with
 *   | [] -> []
 *   | ExhaustiveSearchState.ProofStep.({rule; prev_depth}) :: _ ->
 *     match prev_depth with
 *     | None -> [rule]
 *     | Some d ->
 *         let steps = List.rev rev_history in
 *         let step_table = Array.of_list steps in
 *         let prefix = extract_prefix_impl step_table d in
 *         List.append prefix [rule] *)

let extract_prefixes ExhaustiveSearchState.({rev_history; _}) =
  let history_len = List.length rev_history in
  let not_visited = Int.Hash_set.of_list (List.init history_len Fn.id) in
  let step_table =
    let steps = List.rev rev_history in
    Array.of_list steps
  in
  let rec impl acc curr_id = function
    | [] -> acc
    | _ :: rest -> (
      match Hash_set.mem not_visited curr_id with
      | false -> impl acc (curr_id - 1) rest
      | true ->
          let prefix_indices = extract_index_prefix_impl step_table curr_id in
          List.iter prefix_indices ~f:(Hash_set.remove not_visited) ;
          let prefix = extract_prefix_impl step_table curr_id in
          let acc = prefix :: acc in
          impl acc (curr_id - 1) rest )
  in
  impl [] (history_len - 1) rev_history

let encode_trace Trace.({rule; subgoal_index}) =
  let pp fmt rule = Fmt.pf fmt "%c" (Rules.char_of_exn rule) in
  match rule.Rule.name with
  | "commute_l" ->
      Fmt.strf "(%a%a(%a(<\\d+>)?%a(<\\d+>)?)*)" pp CommuteRule.commute_l
        pp_subgoal_index subgoal_index pp CommuteRule.commute_l pp
        CommuteRule.commute_l
  | "commute1_l" ->
      Fmt.strf "(%a%a(%a(<\\d+>)?%a(<\\d+>)?)*)" pp CommuteRule.commute1_l
        pp_subgoal_index subgoal_index pp CommuteRule.commute1_l pp
        CommuteRule.commute1_l
  | "commute_r" ->
      Fmt.strf "(%a%a(%a(<\\d+>)?%a(<\\d+>)?)*)" pp CommuteRule.commute_r
        pp_subgoal_index subgoal_index pp CommuteRule.commute_r pp
        CommuteRule.commute_r
  | "commute1_r" ->
      Fmt.strf "(%a%a(%a(<\\d+>)?%a(<\\d+>)?)*)" pp CommuteRule.commute1_r
        pp_subgoal_index subgoal_index pp CommuteRule.commute1_r pp
        CommuteRule.commute1_r
  | "extend" ->
      Fmt.strf "((%a%a)?)" pp ExtendRule.extend pp_subgoal_index subgoal_index
  | "blastseq" ->
      Fmt.strf "(%a|%a)%a" pp BlastseqTactic.blastseq pp AutoseqTactic.autoseq
        pp_subgoal_index subgoal_index
  | "sync_n" ->
      Fmt.strf "(%a|%a|%a|%a|%a|%a)%a" pp SyncRule.sync_n pp SyncRule.sync_l pp
        PartialSyncRule.psync_l pp SyncRule.sync_r pp PartialSyncRule.psync_r
        pp SyncRule.sync pp_subgoal_index subgoal_index
  | "sync_l" | "psync_l" ->
      Fmt.strf "(%a|%a|%a)%a" pp SyncRule.sync_l pp PartialSyncRule.psync_l pp
        SyncRule.sync pp_subgoal_index subgoal_index
  | "sync_r" | "psync_r" ->
      Fmt.strf "(%a|%a|%a)%a" pp SyncRule.sync_r pp PartialSyncRule.psync_r pp
        SyncRule.sync pp_subgoal_index subgoal_index
  | _ ->
      Fmt.strf "%c%a" (Rules.char_of_exn rule) pp_subgoal_index subgoal_index

let extract_conflict_specs steps blame_indices =
  let step_table = Array.of_list steps in
  let traces = List.map blame_indices ~f:(extract_prefix_impl step_table) in
  let minimize traces =
    let strs = List.map traces ~f:encode_trace in
    let strs = List.concat [["^"]; strs; [".*$"]] in
    Re2.create_exn (String.concat strs ~sep:"")
  in
  List.map traces ~f:minimize

let add_conflict conflict_cache search_state blames =
  match blames with
  | [] -> (* Cannot analyze blames if there is none *)
          ()
  | _ ->
      let history = ExhaustiveSearchState.history_of search_state in
      let conflict_specs = extract_conflict_specs history blames in
      Logs.debug (fun m ->
          m "blames = [ %a ]" (Fmt.list ~sep:Fmt.comma Fmt.int) blames ) ;
      Logs.debug (fun m ->
          m "conflict spec = %a" ConflictSpecs.pp conflict_specs ) ;
      ConflictCache.add_specs conflict_cache conflict_specs

let check_conflict conflict_cache search_state =
  (* Extract all paths in the tree *)
  let prefixes = extract_prefixes search_state in
  ConflictCache.lookup conflict_cache prefixes
