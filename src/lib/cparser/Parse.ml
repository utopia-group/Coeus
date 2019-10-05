(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)
(* Entry point for the library: parse, elaborate, and transform *)

module CharSet = Set.Make (struct
  type t = char

  let compare = compare
end)

let transform_program t p name =
  let run_pass pass flag p = if CharSet.mem flag t then pass p else p in
  let p1 =
    run_pass StructPassing.program 's'
      (run_pass PackedStructs.program 'p'
         (run_pass Unblock.program 'b' (run_pass Bitfields.program 'f' p)))
  in
  Rename.program p1

let parse_transformations s =
  let t = ref CharSet.empty in
  let set s = String.iter (fun c -> t := CharSet.add c !t) s in
  String.iter
    (function
        | 'b' -> set "b"
        | 's' -> set "s"
        | 'f' -> set "bf"
        | 'p' -> set "bp"
        | _ -> ())
    s ;
  !t

let get_simple_transformation () =
  let t = CharSet.empty in
  let t = CharSet.add 'b' t in
  CharSet.add 'f' t

let read_file sourcefile =
  let ic = open_in_bin sourcefile in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic ; text

let parse_c_file name sourcefile =
  Diagnostics.reset () ;
  let text = read_file sourcefile in
  let p =
    let t = get_simple_transformation () in
    let rec inf = Datatypes.S inf in
    let ast : Cabs.definition list =
      Obj.magic
        ( match
            Parser.translation_unit_file inf (Lexer.tokens_stream name text)
          with
        | Parser.Parser.Inter.Fail_pr ->
            (* Theoretically impossible : implies inconsistencies
                    between grammars. *)
            Diagnostics.fatal_error Diagnostics.no_loc
              "Internal error while parsing"
        | Parser.Parser.Inter.Timeout_pr -> assert false
        | Parser.Parser.Inter.Parsed_pr (ast, _) -> ast )
    in
    let p1 = Elab.elab_file ast in
    Diagnostics.check_errors () ;
    transform_program t p1 name
  in
  Diagnostics.check_errors () ;
  p
