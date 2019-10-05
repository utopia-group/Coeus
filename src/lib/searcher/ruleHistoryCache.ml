open Core
open Prover

module RuleIndexList = struct
  type t = int list [@@deriving sexp, compare, hash]

  let to_rules indices =
    List.map indices ~f:(fun i -> Rules.candidate_rules.(i))

  let of_rules rules =
    Result.all
      (List.map rules ~f:(fun rule ->
           let rule_name = rule.Rule.name in
           match
             Array.findi Rules.candidate_rule_names ~f:(fun _ name ->
                 String.equal name rule_name )
           with
           | Some (idx, _) -> Result.Ok idx
           | None ->
               let msg =
                 Fmt.strf "Cannot find rule with name \"%s\"" rule_name
               in
               Result.Error msg ))

  let of_rules_exn rules =
    match of_rules rules with
    | Result.Ok k -> k
    | Result.Error msg -> raise (Not_found_s (Sexp.Atom msg))
end

module Key = struct
  type t = int * RuleIndexList.t [@@deriving sexp, compare, hash]
end

module type S = sig
  type 'a t

  val get : 'a t -> key:Key.t -> 'a option

  val set : 'a t -> key:Key.t -> data:'a -> unit

  val to_alist : 'a t -> (Key.t * 'a) list

  val length : 'a t -> int
end

module KeepAll = struct
  open Util

  type 'a t = (Key.t, 'a) Cache.Keep_all.t

  let create () = Cache.Keep_all.create None

  let get t ~key = Cache.Keep_all.find t key

  let set = Cache.Keep_all.add

  let to_alist = Cache.Keep_all.to_alist

  let length = Cache.Keep_all.length
end

module Lru = struct
  open Util

  type 'a t = (Key.t, 'a) Cache.Lru.t

  let create ~size = Cache.Lru.create None size

  let get t ~key = Cache.Lru.find t key

  let set = Cache.Lru.add

  let to_alist = Cache.Lru.to_alist

  let length = Cache.Lru.length
end

module MakeSerializer (C : S) (V : Sexpable.S) = struct
  type data_t = V.t [@@deriving sexp]

  type cache_t = data_t C.t

  let store cache file =
    let cache_entries = C.to_alist cache in
    let sexps = List.map cache_entries ~f:[%sexp_of : Key.t * data_t] in
    Sexp.save_sexps_mach file sexps

  let load file cache =
    let cache_entries = Sexp.load_sexps file in
    List.iter cache_entries ~f:(fun sexp ->
        let key, data = [%of_sexp : Key.t * data_t] sexp in
        C.set cache ~key ~data )
end

let memoize ?lru_size ~f =
  match lru_size with
  | Some size -> Util.Cache.memoize ~expire:(`Lru size) f
  | None -> Util.Cache.memoize ~expire:`Keep_all f
