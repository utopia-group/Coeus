open Core
open Ast.Coeus

let fresh_proc_name prog base =
  let counter = ref 0 in
  let rec fresh_impl str =
    let id = Identifier.of_string str in
    match lookup_proc prog id with
    | None -> id
    | Some _ ->
        let new_str = Fmt.strf "%s%d" str !counter in
        incr counter ; fresh_impl new_str
  in
  let start = Fmt.strf "%a_dup" Identifier.pp base in
  fresh_impl start

let duplicate_proc prog (proc: Procedure.t) =
  let proc_name' = fresh_proc_name prog proc.name in
  let proc' = {proc with name= proc_name'} in
  let prog' = update_proc prog proc' in
  Logs.debug (fun m ->
      m "EntryDuplicator duplicated procedure %a to %a" Identifier.pp proc.name
        Identifier.pp proc_name' ) ;
  (prog', proc')

let run prog =
  Logs.debug (fun m -> m "EntryDuplicator starts") ;
  let entry : EntrySpec.t = prog.entry in
  match lookup_proc prog entry.left with
  | None ->
      let msg =
        Fmt.strf "Cannot find left entry procedure \"%a\"" Identifier.pp
          entry.left
      in
      Error msg
  | Some lentry ->
    match lookup_proc prog entry.right with
    | None ->
        let msg =
          Fmt.strf "Cannot find right entry procedure \"%a\"" Identifier.pp
            entry.right
        in
        Error msg
    | Some _ ->
        if Identifier.equal entry.left entry.right then
          let prog, new_proc = duplicate_proc prog lentry in
          let entry = {entry with right= new_proc.name} in
          let prog = {prog with entry} in
          Result.Ok prog
        else Result.Ok prog
