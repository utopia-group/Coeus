open Core
open Ast.Coeus
module SCCDetector = Graph.Components.Make (CallGraph)
module Topological = Graph.Topological.Make (CallGraph)

exception TransformError of string

let create_var_collection (proc: Procedure.t) =
  let open FreshIdentifierGenerator in
  let vars = VarCollection.create () in
  let add_binding VarBinding.({name; _}) = VarCollection.add vars name in
  List.iter proc.params ~f:add_binding ;
  List.iter proc.rets ~f:add_binding ;
  List.iter proc.locals ~f:add_binding ;
  vars

(* let inline_proc vars_map (callee_proc: Procedure.t) (caller_proc: Procedure.t) =
 *   let vars = lookup_vars_cache vars_map caller_proc in
 *   let rename_map = Identifier.Table.create ~size:64 () in
 *   let process_binding VarBinding.({name; ty}) =
 *     let name_str = Fmt.strf "inlined_%a" Identifier.pp name in
 *     let name' = FreshIdentifierGenerator.get_fresh_local name_str vars in
 *     Identifier.Table.set rename_map ~key:name ~data:(Expr.VarExpr name') ;
 *     VarBinding.{name= name'; ty}
 *   in
 *   let callee_ret_bindings = List.map callee_proc.rets ~f:process_binding in
 *   let callee_local_bindings = List.map callee_proc.locals ~f:process_binding in
 *   (* For parameters, we apply substitutions directly *)
 *   match List.zip 
 *   failwith "" *)
(* let inline_procs vars_map prog callee callers =
 *   let callee_proc = lookup_proc_exn prog callee in
 *   List.fold callers ~init:prog ~f:(fun acc caller ->
 *       let caller_proc = lookup_proc_exn acc caller in
 *       let caller_proc' = inline_proc vars_map callee_proc caller_proc in
 *       update_proc acc caller_proc' ) *)

let find_inline_candidates cg imap scc =
  match scc with
  | [] ->
      let msg = "[INTERNAL] Found an SCC with zero vertex" in
      failwith msg
  | callee :: rest ->
    (* Skip inlining if the SCC contains more than one node (i.e. mutual recursive procs) *)
    match rest with
    | _ :: _ -> imap
    | [] ->
      (* Check for self-loop (i.e. procedure calls itself *)
      match CallGraph.mem_edge cg callee callee with
      | true -> imap
      | false ->
        match CallGraph.pred cg callee with
        | [] -> imap
        | _ as callers ->
            let add_candidate imap caller =
              let candidate_set =
                match Identifier.Map.find imap caller with
                | None -> Identifier.Set.empty
                | Some cset -> cset
              in
              let candidate_set' = Identifier.Set.add candidate_set callee in
              Identifier.Map.set imap ~key:caller ~data:candidate_set'
            in
            List.fold callers ~init:imap ~f:add_candidate

let inline_proc prog caller callees =
  let caller_proc = lookup_proc_exn prog caller in
  let vars = create_var_collection caller_proc in
  let rec process_stmt stmt =
    let open Stmt in
    match stmt with
    | Call {rets; name= callee; args} when Identifier.Set.mem callees callee
      -> (
      match lookup_proc prog callee with
      | None ->
          let msg =
            Fmt.strf "Cannot find call stmt target \"%a\" (called from %a)"
              Identifier.pp callee Identifier.pp caller
          in
          raise (TransformError msg)
      | Some callee_proc ->
          let var_rename_map = Identifier.Table.create ~size:64 () in
          let process_binding VarBinding.({name; ty}) =
            let vb_name = Identifier.string_of name in
            let name_str =
              (* Avoid ugly names with multiple prefixes *)
              if String.is_prefix vb_name ~prefix:"inlined_" then vb_name
              else Fmt.strf "inlined_%s" vb_name
            in
            let name' =
              FreshIdentifierGenerator.get_fresh_local name_str vars
            in
            Identifier.Table.set var_rename_map ~key:name ~data:name' ;
            VarBinding.{name= name'; ty}
          in
          let callee_param_bindings =
            List.map callee_proc.params ~f:process_binding
          in
          let callee_ret_bindings =
            List.map callee_proc.rets ~f:process_binding
          in
          let callee_local_bindings =
            List.map callee_proc.locals ~f:process_binding
          in
          (* For parameters, we apply substitutions directly *)
          match List.zip callee_param_bindings args with
          | None ->
              raise
                (TransformError
                   "ProcInliner found mismatched actual and formal params")
          | Some pairs ->
              let param_assigns =
                List.map pairs ~f:(fun (vb, arg) ->
                    Assign {lhs= Lvalue.of_var vb.name; rhs= arg} )
              in
              match List.zip callee_ret_bindings rets with
              | None ->
                  raise
                    (TransformError
                       "ProcInliner found mismatched actual and formal rets")
              | Some pairs ->
                  let ret_assigns =
                    List.map pairs ~f:(fun (vb, ret) ->
                        Assign {lhs= ret; rhs= Expr.VarExpr vb.name} )
                  in
                  let inline_body =
                    Stmts.map_var callee_proc.stmts ~f:(fun v ->
                        match Identifier.Table.find var_rename_map v with
                        | Some v' -> v'
                        | None -> v )
                  in
                  let new_bindings =
                    List.concat
                      [ callee_param_bindings
                      ; callee_ret_bindings
                      ; callee_local_bindings ]
                  in
                  let inlined_stmts =
                    List.concat [param_assigns; inline_body; ret_assigns]
                  in
                  (new_bindings, inlined_stmts) )
    | If {cond; then_branch; else_branch} ->
        let vbs_then, then_branch = process_stmts then_branch in
        let vbs_else, else_branch = process_stmts else_branch in
        (List.append vbs_then vbs_else, [If {cond; then_branch; else_branch}])
    | While {cond; body} ->
        let vbs, body = process_stmts body in
        (vbs, [While {cond; body}])
    | For {counter; lower; upper; step; direction; body} ->
        let vbs, body = process_stmts body in
        (vbs, [For {counter; lower; upper; step; direction; body}])
    | _ -> ([], [stmt])
  and process_stmts stmts =
    let new_bindings, rev_stmts =
      List.fold stmts ~init:([], []) ~f:(fun (acc_vbs, acc_stmts) stmt ->
          let vbs, stmts' = process_stmt stmt in
          let acc_vbs' = List.append vbs acc_vbs in
          let acc_stmts' = List.rev_append stmts' acc_stmts in
          (acc_vbs', acc_stmts') )
    in
    (new_bindings, List.rev rev_stmts)
  in
  let new_bindings, stmts = process_stmts caller_proc.stmts in
  let new_caller_proc =
    let open Procedure in
    { caller_proc with
      locals= List.append new_bindings caller_proc.locals; stmts }
  in
  update_proc prog new_caller_proc

let process_prog cg prog =
  let sccs = SCCDetector.scc_list cg in
  (* The first phase detects which procedures to inline *)
  let inline_map =
    List.fold sccs ~init:Identifier.Map.empty ~f:(find_inline_candidates cg)
  in
  (* The second phase performs the actual inlining *)
  (* We want to process the procedures in reverse topological order *)
  let inline_records =
    Topological.fold
      (fun caller acc ->
        match Identifier.Map.find inline_map caller with
        | None -> acc
        | Some callees -> (caller, callees) :: acc )
      cg []
  in
  List.fold inline_records ~init:prog ~f:(fun acc (caller, callees) ->
      inline_proc acc caller callees )

let run prog =
  try
    Logs.debug (fun m -> m "ProcInliner starts") ;
    let cg = CallGraph.Builder.of_coeus prog in
    let prog' = process_prog cg prog in
    Logs.debug (fun m -> m "ProcInliner successfully finished") ;
    Result.Ok prog'
  with TransformError msg -> Result.Error msg
