open Core
open Ast.Ecoeus

(* The VeriMAP tool has poor support for array-typed arguments. *)
(* This pass tries to "lift" the array arguments to a "global" by renaming aliasing arrays to have the same name. *)

exception TransformError of string

module RenameEnv = struct
  module RenameTable = struct
    type t = Identifier.t Identifier.Table.t

    let create () = Identifier.Table.create ()

    let add table id new_id =
      match Identifier.Table.add table ~key:id ~data:new_id with
      | `Ok -> ()
      | `Duplicate ->
          let old_id = Identifier.Table.find_exn table id in
          if not (Identifier.equal old_id new_id) then
            let msg =
              Fmt.strf
                "Find conflicting param assignments: both \"%a\" and \"%a\" \
                 get passed to \"%a\""
                Identifier.pp old_id Identifier.pp new_id Identifier.pp id
            in
            raise (TransformError msg)
          else ()

    let lookup = Identifier.Table.find
  end

  type t = RenameTable.t Identifier.Table.t

  let create () = Identifier.Table.create ()

  let add env proc_name id new_id =
    Identifier.Table.update env proc_name ~f:(fun opt_table ->
        let table =
          match opt_table with
          | Some table -> table
          | None -> RenameTable.create ()
        in
        RenameTable.add table id new_id ;
        table )

  let var_subst_of_proc env proc_name =
    match Identifier.Table.find env proc_name with
    | None -> Fn.id
    | Some table ->
        fun id ->
          match RenameTable.lookup table id with Some id' -> id' | None -> id
end

let split_procs ast =
  let left_entry = ast.entry.left in
  let left_entry_proc = lookup_proc_exn ast left_entry in
  let right_entry = ast.entry.right in
  let right_entry_proc = lookup_proc_exn ast right_entry in
  let left_proc_names, right_proc_names =
    let open Frontend in
    let call_graph = CallGraph.Builder.of_ecoeus ast in
    let left_procs, right_procs =
      ( ProcReachability.reachable_procs call_graph [left_entry]
      , ProcReachability.reachable_procs call_graph [right_entry] )
    in
    ( List.filter left_procs ~f:(fun p -> not (Identifier.equal p left_entry))
    , List.filter right_procs ~f:(fun p -> not (Identifier.equal p right_entry))
    )
  in
  let left_procs = List.map left_proc_names ~f:(lookup_proc_exn ast) in
  let right_procs = List.map right_proc_names ~f:(lookup_proc_exn ast) in
  (left_entry_proc, left_procs, right_entry_proc, right_procs)

let run_impl ast =
  let env = RenameEnv.create () in
  let build_env (proc: Procedure.t) =
    let rec process_stmt stmt =
      let open Stmt in
      match stmt.bare_stmt with
      | Call {name; args; _} ->
          let target_proc = lookup_proc_exn ast name in
          List.iteri args ~f:(fun idx arg ->
              let arg_ty = Expr.type_of arg in
              if not (Type.is_array_type arg_ty) then ()
              else
                match arg.Expr.bare_expr with
                | Expr.VarExpr VarBinding.({name= new_name; _}) ->
                    let param = List.nth_exn target_proc.params idx in
                    let old_name = param.VarBinding.name in
                    RenameEnv.add env name old_name new_name
                | _ ->
                    let msg =
                      Fmt.strf "Unsupported array argument: \"%a\"" Expr.pp arg
                    in
                    raise (TransformError msg) )
      | _ -> ()
    and process_stmts stmts = List.iter stmts ~f:process_stmt in
    process_stmts proc.stmts
  in
  let apply_env (proc: Procedure.t) =
    let var_subst_f = RenameEnv.var_subst_of_proc env proc.name in
    let params =
      List.map proc.params ~f:(fun vb ->
          let name = var_subst_f vb.VarBinding.name in
          {vb with name} )
    in
    let stmts = Stmts.map_var proc.stmts ~f:var_subst_f in
    {proc with params; stmts}
  in
  List.iter ast.procs ~f:build_env ;
  List.fold ast.procs ~init:ast ~f:(fun acc proc ->
      let proc' = apply_env proc in
      update_proc acc proc' )

let run ast =
  let _, left_procs, _, right_procs = split_procs ast in
  if
    List.for_all (List.append left_procs right_procs) ~f:(fun p ->
        not
          (List.exists p.Procedure.params ~f:(fun VarBinding.({ty; _}) ->
               Type.is_array_type ty )) )
  then Result.Ok ast
  else
    try
      let ast' = run_impl ast in
      Result.Ok ast'
    with TransformError msg -> Result.Error msg
