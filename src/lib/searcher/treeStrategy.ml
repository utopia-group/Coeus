open Core
open Prover

type t = SearchConfig.t -> ProverState.t -> (Rule.t * ProverState.t) list

(* The general search framework *)
module ExpandStatus = struct
  type ('a, 'b) t = Terminal of 'a | NonTerminal of 'b
end

module type S = sig
  (** Local search state *)
  type t

  (** Global search state *)
  type u

  val init : SearchConfig.t -> ProverState.t -> u * t

  val compare : t -> t -> int

  val expand : u -> t -> (SearchResult.t, t list) ExpandStatus.t
end

module type W = functor (State : S) -> sig
  (** Type of the working set *)
  type t

  val init : State.t -> t

  val add : t -> State.t -> unit

  val take : t -> State.t option
end

module Make (WorkSetMaker : W) (State : S) = struct
  module WorkSet = WorkSetMaker (State)

  let search search_config init_prover_state =
    let global_state, init_local_state =
      State.init search_config init_prover_state
    in
    let work_set = WorkSet.init init_local_state in
    let rec do_search () =
      match WorkSet.take work_set with
      | None ->
          let msg = "Search space has been exhausted and no proof was found" in
          Result.Error msg
      | Some work_item -> (
        match State.expand global_state work_item with
        | ExpandStatus.Terminal res -> Result.Ok res
        | ExpandStatus.NonTerminal next_items ->
            List.iter next_items ~f:(WorkSet.add work_set) ;
            do_search () )
    in
    do_search ()
end

(* General search extended with conflict analysis *)

module type L = sig
  type t

  val init : SearchConfig.t -> ProverState.t -> t

  val compare : t -> t -> int

  val search_state_of : t -> ExhaustiveSearchState.t

  val expand : int option -> t -> SearchConfig.t -> ProverState.t -> t list
end

module ConflictGlobalState = struct
  type t =
    { search_config: SearchConfig.t
    ; conflict_cache: ConflictAnalysis.ConflictCache.t
    ; mutable failed_count: int
    ; mutable blame_count: int
    ; mutable timeout_count: int
    ; mutable block_count: int }

  let init search_config =
    let conflict_cache =
      ConflictAnalysis.ConflictCache.create
        ~size:search_config.SearchConfig.max_conflict
    in
    { search_config
    ; conflict_cache
    ; failed_count= 0
    ; blame_count= 0
    ; timeout_count= 0
    ; block_count= 0 }
end

module MakeConflictState (LocalState : L) = struct
  type t = LocalState.t

  type u = ConflictGlobalState.t

  let init search_config init_state =
    let global = ConflictGlobalState.init search_config in
    let local = LocalState.init search_config init_state in
    (global, local)

  let compare = LocalState.compare

  let expand (global_state : ConflictGlobalState.t)
      (local_state : LocalState.t) =
    let max_conflict = global_state.search_config.max_conflict in
    let conflict_cache = global_state.conflict_cache in
    let search_config = global_state.search_config in
    let search_state = LocalState.search_state_of local_state in
    let open ExhaustiveSearchState in
    if
      max_conflict > 0
      && ConflictAnalysis.check_conflict conflict_cache search_state
    then (
      Logs.info (fun m ->
          m "Conflict Analysis blocked current solution: %a" pp_rules
            search_state ) ;
      global_state.block_count <- global_state.block_count + 1 ;
      ExpandStatus.NonTerminal [] )
    else
      let prover_state = search_state.prover_state in
      match ProverState.current_goal_of prover_state with
      | None -> (
        match try_discharge search_config search_state with
        | DischargeResult.Discharged rules ->
            let res =
              let open SearchResult in
              { rules
              ; failed_discharges= global_state.failed_count
              ; num_blames= global_state.blame_count
              ; timeout_discharges= global_state.timeout_count
              ; blocked_conflicts= global_state.block_count }
            in
            ExpandStatus.Terminal res
        | DischargeResult.Blamed blames ->
            if max_conflict > 0 && not (List.is_empty blames) then (
              ConflictAnalysis.add_conflict conflict_cache search_state blames ;
              global_state.blame_count <- global_state.blame_count + 1 ) ;
            global_state.failed_count <- global_state.failed_count + 1 ;
            ExpandStatus.NonTerminal []
        | DischargeResult.Timeout ->
            global_state.timeout_count <- global_state.timeout_count + 1 ;
            ExpandStatus.NonTerminal []
        | DischargeResult.NotDischarged ->
            global_state.failed_count <- global_state.failed_count + 1 ;
            ExpandStatus.NonTerminal [] )
      | Some goal ->
          let prev_depth = goal.Goal.prev_step in
          let next_local_states =
            LocalState.expand prev_depth local_state search_config prover_state
          in
          ExpandStatus.NonTerminal next_local_states
end

(* Work set implementations *)

module DFSWorkSet (State : S) = struct
  type t = State.t Linked_stack.t

  let init state =
    let stack = Linked_stack.create () in
    Linked_stack.push stack state ;
    stack

  let add = Linked_stack.push

  let take = Linked_stack.pop
end

module BFSWorkSet (State : S) = struct
  type t = State.t Linked_queue.t

  let init state =
    let queue = Linked_queue.create () in
    Linked_queue.enqueue queue state ;
    queue

  let add = Linked_queue.enqueue

  let take = Linked_queue.dequeue
end

(* Local state implementations *)

let gen_local_state (tree_strategy : t) =
  ( module struct
    type t = ExhaustiveSearchState.t

    let init _ prover_state = ExhaustiveSearchState.init prover_state

    let search_state_of s = s

    (* We don't need to compare states in BFS/DFS *)
    let compare _ _ = 0

    let expand prev_depth search_state search_config prover_state =
      let open ExhaustiveSearchState in
      let transitions = tree_strategy search_config prover_state in
      List.map transitions ~f:(fun (rule, prover_state) ->
          let subgoal_index =
            ProverState.current_subgoal_index_of prover_state
          in
          let step = ProofStep.{rule; prev_depth; subgoal_index} in
          let rev_history = step :: search_state.rev_history in
          ExhaustiveSearchState.{prover_state; rev_history} )
  end
  : L )

let dfs tree_strategy =
  let module LocalState = (val gen_local_state tree_strategy) in
  let module SearchState = MakeConflictState (LocalState) in
  let module Search = Make (DFSWorkSet) (SearchState) in
  Search.search

let bfs (tree_strategy : t) =
  let module LocalState = (val gen_local_state tree_strategy) in
  let module SearchState = MakeConflictState (LocalState) in
  let module Search = Make (BFSWorkSet) (SearchState) in
  Search.search
