open Core
open Ast.Coeus

module VarCollection = struct
  type t = {vars: Identifier.Hash_set.t; counter: int ref}

  let create ?(size= 16) () =
    {vars= Identifier.Hash_set.create ~size (); counter= ref 0}

  let of_list vs = {vars= Identifier.Hash_set.of_list vs; counter= ref 0}

  let add {vars; _} id = Hash_set.add vars id

  let strict_add {vars; _} id = Hash_set.strict_add vars id

  let mem {vars; _} id = Hash_set.mem vars id

  let get_and_incr_counter {counter; _} =
    let res = !counter in
    incr counter ; res
end

let rec get_fresh_local name vars =
  let cnt = VarCollection.get_and_incr_counter vars in
  let name = Fmt.strf "%s.%d" name cnt in
  let id = Identifier.of_string name in
  match VarCollection.strict_add vars id with
  | Result.Ok _ -> id
  | Result.Error _ -> get_fresh_local name vars

let rename_fresh_local id vars =
  match VarCollection.strict_add vars id with
  | Result.Ok _ -> id
  | Result.Error _ ->
      let str = Identifier.string_of id in
      get_fresh_local str vars
