module IsApplicable = Rule.IsApplicable

type t = ProverConfig.t -> ProverState.t -> Rule.t option

let mk_rule ?is_applicable ?(is_aggressive= false) ~name tactic =
  let apply config state =
    match tactic config state with
    | None -> Result.Error "Trying to apply a non-applicable tactic"
    | Some rule ->
        let rec apply_impl acc (rule: Rule.t) =
          match rule.apply config acc with
          | Result.Error msg ->
              let msg = Fmt.strf "Failed tactic application: %s" msg in
              Result.Error msg
          | Result.Ok acc' ->
              let acc' = ProverState.resolve_top acc' in
              match tactic config acc' with
              | None -> Result.Ok acc'
              | Some rule' -> apply_impl acc' rule'
        in
        apply_impl state rule
  in
  let apply = Rule.mk_apply apply in
  let is_applicable =
    match is_applicable with
    | None -> Rule.mk_default_is_applicable apply
    | Some f -> Rule.mk_is_applicable f
  in
  Rule.{name; is_applicable; is_aggressive; apply}
