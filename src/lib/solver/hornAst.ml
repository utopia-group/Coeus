open Core
open Ast.Ecoeus

module Predicate = struct
  type t = {name: Identifier.t; params: VarBinding.t list; hints: Expr.t list}
  [@@deriving sexp, compare, hash]

  let pp_hint fmt e = Fmt.pf fmt "@[<hov 2>hint %a;@]" Expr.pp e

  let pp_hints fmt = function
    | [] -> Fmt.pf fmt ";"
    | hints ->
        Fmt.pf fmt ":@.  @[<v 2>%a@]" (Fmt.list ~sep:Fmt.sp pp_hint) hints

  let pp fmt {name; params; hints} =
    Fmt.pf fmt "@[<hov 2>predicate %a (%a)%a@]" Identifier.pp name
      (Fmt.list ~sep:Fmt.comma VarBinding.pp)
      params pp_hints hints

  module Apply = struct
    type t = {name: Identifier.t; args: Expr.t list}
    [@@deriving sexp, compare, hash]

    let pp fmt {name; args} =
      Fmt.pf fmt "%a(%a)" Identifier.pp name
        (Fmt.list ~sep:(fun fmt () -> Fmt.string fmt ", ") Expr.pp)
        args
  end
end

module Rule = struct
  module Head = struct
    type t = False | Predicate of Predicate.Apply.t
    [@@deriving sexp, compare, hash]

    let predicates_of_impl pred_set = function
      | False -> ()
      | Predicate {name; _} -> Hash_set.add pred_set name

    let pp fmt = function
      | False -> Fmt.string fmt "false"
      | Predicate apply -> Predicate.Apply.pp fmt apply
  end

  module Body = struct
    type t = Expr of Expr.t | Predicate of Predicate.Apply.t
    [@@deriving sexp, compare, hash]

    let subst_predicate ~f = function
      | Expr e -> e
      | Predicate Predicate.Apply.({name; args}) -> f name args

    let predicates_of_impl pred_set = function
      | Expr _ -> ()
      | Predicate Predicate.Apply.({name; _}) -> Hash_set.add pred_set name

    let predicate_set_of body =
      let pred_set = Identifier.Hash_set.create () in
      predicates_of_impl pred_set body ;
      pred_set

    let predicates_of body = Hash_set.to_list (predicate_set_of body)

    let pp fmt = function
      | Expr e -> Expr.pp fmt e
      | Predicate apply -> Predicate.Apply.pp fmt apply
  end

  module Bodies = struct
    type t = Body.t list [@@deriving sexp, compare, hash]

    let subst_predicate ~f = List.map ~f:(Body.subst_predicate ~f)

    let predicate_set_of bodies =
      let pred_set = Identifier.Hash_set.create () in
      List.iter bodies ~f:(Body.predicates_of_impl pred_set) ;
      pred_set

    let predicates_of bodies = Hash_set.to_list (predicate_set_of bodies)

    let pp =
      let pp_conjunct fmt () = Fmt.pf fmt " &&@ " in
      Fmt.list ~sep:pp_conjunct Body.pp
  end

  type t =
    { name: Identifier.t
    ; bindings: VarBinding.t list
    ; head: Head.t
    ; bodies: Bodies.t }
  [@@deriving sexp, compare, hash]

  let predicate_set_of {head; bodies; _} =
    let pred_set = Identifier.Hash_set.create () in
    Head.predicates_of_impl pred_set head ;
    List.iter bodies ~f:(Body.predicates_of_impl pred_set) ;
    pred_set

  let predicates_of exprs = Hash_set.to_list (predicate_set_of exprs)

  let pp fmt {name; bindings; head; bodies} =
    Fmt.pf fmt "@[<hov 2>rule %a@ (%a):@ @[<2>%a <-@ %a@];@]" Identifier.pp
      name
      (Fmt.list ~sep:Fmt.comma VarBinding.pp)
      bindings Head.pp head Bodies.pp bodies
end

type t =
  { pred_map: Predicate.t Identifier.Map.t
  ; rule_map: Rule.t Identifier.Map.t
  ; rule_index: Rule.t list Identifier.Map.t }

let pred_env_of {pred_map; _} = pred_map

let preds_of {pred_map; _} = Identifier.Map.data pred_map

let num_preds {pred_map; _} = Identifier.Map.length pred_map

let lookup_pred {pred_map; _} = Identifier.Map.find pred_map

let lookup_pred_exn {pred_map; _} = Identifier.Map.find_exn pred_map

let rules_of {rule_map; _} = Identifier.Map.data rule_map

let num_rules {rule_map; _} = Identifier.Map.length rule_map

let lookup_rule {rule_map; _} = Identifier.Map.find rule_map

let lookup_rule_exn {rule_map; _} = Identifier.Map.find_exn rule_map

let rules_of_head {rule_index; _} pred_name =
  match Identifier.Map.find rule_index pred_name with
  | None -> []
  | Some rules -> rules

let create preds rules =
  let create_from_alist item alist =
    match Identifier.Map.of_alist alist with
    | `Duplicate_key id ->
        let msg =
          Fmt.strf
            "[INTERNAL] Duplicate %s definition in horn ast construction: %a"
            item Identifier.pp id
        in
        failwith msg
    | `Ok amap -> amap
  in
  let pred_alist =
    List.map preds ~f:(fun pred -> (pred.Predicate.name, pred))
  in
  let pred_map = create_from_alist "predicate" pred_alist in
  let rule_alist = List.map rules ~f:(fun rule -> (rule.Rule.name, rule)) in
  let rule_map = create_from_alist "rule" rule_alist in
  let rule_index =
    List.fold rules ~init:Identifier.Map.empty ~f:(fun acc rule ->
        let rule_name =
          match rule.Rule.head with
          | Rule.Head.False -> Identifier.of_string "false"
          | Rule.Head.Predicate {name; _} -> name
        in
        Identifier.Map.update acc rule_name ~f:(function
          | None -> [rule]
          | Some rules -> rule :: rules ) )
  in
  {pred_map; rule_map; rule_index}

let pp fmt ast =
  let preds = preds_of ast in
  let rules = rules_of ast in
  Fmt.pf fmt "@[<v>%a@,%a@]" (Fmt.list Predicate.pp) preds (Fmt.list Rule.pp)
    rules
