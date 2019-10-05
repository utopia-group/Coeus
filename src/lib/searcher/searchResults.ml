open Core

module SearchStatus = struct
  type t = Fail | Timeout | Succ of float * SearchResult.t
  [@@deriving sexp, compare]

  let json_of_succ time res =
    match SearchResult.json_of res with
    | `Assoc alist ->
        let time_entry = ("time_sec", `Float time) in
        `Assoc (List.append alist [time_entry])
    | _ as json ->
        let msg =
          Fmt.strf "Unexpected search result json: %a"
            (Yojson.Safe.pretty_print ~std:true)
            json
        in
        failwith msg

  let json_of status =
    let open Yojson.Safe in
    match status with
    | Fail -> `String "fail"
    | Timeout -> `String "timeout"
    | Succ (time, res) -> json_of_succ time res

  let of_json json =
    let open Yojson.Safe in
    let open Result in
    match json with
    | `String "fail" -> Ok Fail
    | `String "timeout" -> Ok Timeout
    | `Assoc alist -> (
      match List.Assoc.find alist ~equal:String.equal "time_sec" with
      | Some (`Float time) ->
          SearchResult.of_json json >>= fun res -> Result.Ok (Succ (time, res))
      | _ -> Result.Error "Cannot find the time_sec field" )
    | _ ->
        let msg =
          Fmt.strf "Cannot parse SearchStatus json: %a"
            (pretty_print ~std:true) json
        in
        Result.Error msg

  let pp fmt = function
    | Fail -> Fmt.string fmt "fail"
    | Timeout -> Fmt.string fmt "timeout"
    | Succ (_, SearchResult.({rules; _})) ->
        Fmt.pf fmt "succ with [ %a ]" Prover.Rule.pp_rules rules
end

type t = SearchStatus.t String.Map.t

let json_of result_map =
  let open Yojson.Safe in
  let alist = String.Map.to_alist ~key_order:`Increasing result_map in
  let alist =
    List.map alist ~f:(fun (name, res) -> (name, SearchStatus.json_of res))
  in
  `Assoc alist

let save file result_map =
  let data = Yojson.Safe.pretty_to_string ~std:true (json_of result_map) in
  Out_channel.write_all file data

let load file =
  let open Yojson.Safe in
  let open Result in
  try
    let json = from_file file in
    all
      (List.map (Util.to_assoc json) ~f:(fun (name, json) ->
           let res = SearchStatus.of_json json in
           map res ~f:(fun res -> (name, res)) ))
    >>= fun entries ->
    match String.Map.of_alist entries with
    | `Ok result_map -> Ok result_map
    | `Duplicate_key k ->
        let msg = Fmt.strf "Duplicate key in json loading: %s" k in
        Result.Error msg
  with Util.Type_error (msg, _) ->
    let msg = Fmt.strf "Json error: %s" msg in
    Result.Error msg
