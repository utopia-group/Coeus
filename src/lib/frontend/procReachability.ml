open Core
open Ast.Coeus
module CallGraphDfs = Graph.Traverse.Dfs (CallGraph)

let reachable_procs call_graph root_procs =
  let reachable_set = Identifier.Hash_set.create () in
  let process_proc proc =
    CallGraphDfs.prefix_component (Hash_set.add reachable_set) call_graph proc
  in
  List.iter root_procs ~f:(fun proc ->
      if
        CallGraph.mem_vertex call_graph proc
        && not (Hash_set.mem reachable_set proc)
      then process_proc proc ) ;
  Hash_set.to_list reachable_set
