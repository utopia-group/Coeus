open Core
open Ast.Coeus

let inv_counter = ref 0

let var_counter = ref 0

let proc_counter = ref 0

let get_fresh_inv ?(rel= false) () =
  let name = if rel then "INV_REL" else "INV" in
  let s = Fmt.strf "%s$%d" name !inv_counter in
  incr inv_counter ; Identifier.of_string s

let get_fresh_var ?(name= "tmpvar") () =
  let s = Fmt.strf "%s$%d" name !var_counter in
  incr var_counter ; Identifier.of_string s

let get_fresh_proc ?(name= "tmpproc") () =
  let s = Fmt.strf "%s$%d" name !proc_counter in
  incr proc_counter ; Identifier.of_string s

let reset () =
  inv_counter := 0 ;
  var_counter := 0 ;
  proc_counter := 0
