open Core
open Ast.Coeus

exception ResolutionError of string

let add_proc_entry proc_map (entry_spec: EntrySpec.t) name proc_sig =
  Identifier.Map.update proc_map name ~f:(function
    | None ->
        let entry_side =
          if Identifier.equal name entry_spec.left then Some Side.Left
          else if Identifier.equal name entry_spec.right then Some Side.Right
          else None
        in
        Env.ProcInfo.{entry_side; proc_sig}
    | Some _ ->
        let msg =
          Fmt.strf "Found procedure with duplicate name \"%a\"" Identifier.pp
            name
        in
        raise (ResolutionError msg) )

let process_bindings pname kind vmap bindings =
  List.fold bindings ~init:vmap ~f:(fun acc VarBinding.({name; ty}) ->
      match Identifier.Map.find vmap name with
      | None ->
          let var_info = Env.VariableInfo.{ty; kind; parent= Some pname} in
          Identifier.Map.set acc ~key:name ~data:var_info
      | Some _ ->
          let msg =
            Fmt.strf
              "Found variable with duplicated name \"%a\". Please run \
               VariableRenamer first."
              Identifier.pp name
          in
          raise (ResolutionError msg) )

let process_proc entry_spec var_map proc_map (proc: Procedure.t) =
  Logs.debug (fun m -> m "Resolving procedure %a..." Identifier.pp proc.name) ;
  let open Env in
  let signature = ProcSignature.of_proc proc in
  let var_map =
    process_bindings proc.name VariableKind.InParam var_map proc.params
  in
  let var_map =
    process_bindings proc.name VariableKind.OutParam var_map proc.rets
  in
  let var_map =
    process_bindings proc.name VariableKind.Local var_map proc.locals
  in
  let proc_map = add_proc_entry proc_map entry_spec proc.name signature in
  Logs.debug (fun m -> m "Procedure %a resolved" Identifier.pp proc.name) ;
  (var_map, proc_map)

let process_decl decl_map decl =
  let name = decl.FunDecl.name in
  Identifier.Map.set decl_map ~key:name ~data:decl

let run prog =
  try
    let decl_map =
      List.fold prog.decls ~init:Identifier.Map.empty ~f:process_decl
    in
    let var_map, proc_map =
      List.fold prog.procs ~init:(Identifier.Map.empty, Identifier.Map.empty)
        ~f:(fun (var_acc, proc_acc) proc ->
          process_proc prog.entry var_acc proc_acc proc )
    in
    let env = Env.{var_map; proc_map; decl_map} in
    Result.Ok env
  with ResolutionError msg -> Result.Error msg
