open Core

type t = string String.Map.t

let set ~key ?(value= "") opt = String.Map.set opt ~key ~data:value

let set_batch key_vals opt =
  List.fold key_vals ~init:opt ~f:(fun acc (key, opt_val) ->
      let value = Option.value ~default:"" opt_val in
      set acc ~key ~value )

let unset ~key opt = String.Map.remove opt key

let unset_batch ~keys opt =
  List.fold keys ~init:opt ~f:(fun acc key -> unset ~key acc)

let mem ~key opt = String.Map.mem opt key

let lookup ~key opt = String.Map.find opt key

let lookup_int ~key opt =
  Option.(lookup ~key opt >>= fun v -> int_of_string_opt v)

let lookup_exn ~key opt =
  match lookup opt ~key with
  | Some v -> v
  | None -> raise (Not_found_s (Sexp.Atom "REPL config lookup failed"))

let empty = String.Map.empty

let of_alist = String.Map.of_alist_reduce ~f:(fun _ x -> x)

let to_alist opt = String.Map.to_alist opt
