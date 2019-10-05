open Core

module IsApplicable = struct
  type t = ProverConfig.t -> ProverState.t -> bool
end

type t =
  { name: string
  ; is_applicable: IsApplicable.t
  ; is_aggressive: bool
  ; apply: ProverConfig.t -> ProverState.t -> (ProverState.t, string) Result.t
  }

let sexp_of_t {name; _} = [%sexp_of: string] name

let t_of_sexp _ =
  failwith "[INTERNAL] Cannot directly construct Rule from sexp"

let compare {name= n0; _} {name= n1; _} = [%compare: string] n0 n1

(* Take care of automatic limit check *)
let mk_apply f config state =
  Result.(
    ProverState.check_limit config state
    >>= fun _ ->
    f config state
    >>= fun state' -> Ok (ProverState.assign_subgoal_indices state'))

(* Take care of automatic limit check*)
let mk_is_applicable f config state =
  if ProverState.exceeds_limit config state then false else f config state

let mk_default_is_applicable f config state = Result.is_ok (f config state)

let pp fmt {name; _} = Fmt.pf fmt "%s" name

let pp_rules = Fmt.list pp ~sep:(fun fmt () -> Fmt.string fmt " ; ")
