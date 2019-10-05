open Core
open Ast.Coeus
module IdentifierGraph = Graph.Persistent.Digraph.ConcreteBidirectional (Identifier)
include IdentifierGraph

module Builder = struct
  let rec process_stmt caller cg stmt =
    let open Stmt in
    match stmt with
    | Assume _ | Assign _ -> cg
    | If {then_branch; else_branch; _} ->
        let cg = process_stmts caller cg then_branch in
        process_stmts caller cg else_branch
    | While {body; _} | For {body; _} -> process_stmts caller cg body
    | Call {name= callee; _} -> IdentifierGraph.add_edge cg caller callee

  and process_stmts caller cg stmts =
    List.fold stmts ~init:cg ~f:(process_stmt caller)

  let process_proc cg (proc: Procedure.t) =
    process_stmts proc.name cg proc.stmts

  let of_coeus prog =
    List.fold prog.procs ~init:IdentifierGraph.empty ~f:process_proc

  let of_ecoeus prog = of_coeus (Ast.Ecoeus.to_coeus prog)
end
