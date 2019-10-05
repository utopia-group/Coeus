open Core
open Ast.Ecoeus

type t =
  { pre_conds: Precondition.t list
        (** Preconditions are stored in reverse order *)
  ; post_cond: Postcondition.t
  ; blame_step: int option
  ; ast_size: int }
[@@deriving sexp, compare, hash]

let mk ?blame_step pre_conds post_cond =
  let ast_size =
    1
    + List.fold pre_conds ~init:0 ~f:(fun acc p ->
          acc + Precondition.ast_size_of p )
    + Postcondition.ast_size_of post_cond
  in
  {pre_conds; post_cond; blame_step; ast_size}

let ast_size_of {ast_size; _} = ast_size

let free_vars_of {pre_conds; post_cond; _} =
  let id_table = Identifier.Table.create ~size:64 () in
  let add_bindings bindings =
    List.iter bindings ~f:(fun VarBinding.({name; ty}) ->
        Identifier.Table.set id_table ~key:name ~data:ty )
  in
  let process_pre pre =
    let open Precondition in
    match pre.bare with
    | Assume es | Predicate {args= es; _} ->
        let vbs = Expr.free_vars_of_exprs es in
        add_bindings vbs
    | Assign (lhs, rhs) ->
        Identifier.Table.remove id_table lhs.base.name ;
        let lhs_vbs = Expr.free_vars_of_exprs lhs.indices in
        let rhs_vbs = Expr.free_vars_of rhs in
        add_bindings lhs_vbs ; add_bindings rhs_vbs
    | Havoc _ -> ()
  in
  let process_post post =
    let bindings = Postcondition.free_vars_of post in
    add_bindings bindings
  in
  process_post post_cond ;
  List.iter pre_conds ~f:process_pre ;
  Identifier.Table.fold id_table ~init:[] ~f:(fun ~key ~data acc ->
      let vb = VarBinding.{name= key; ty= data} in
      vb :: acc )

let preds_of_impl pred_set {pre_conds; post_cond; _} =
  List.iter pre_conds ~f:(fun pre ->
      match pre.bare with
      | Precondition.Predicate {name; _} -> Hash_set.add pred_set name
      | _ -> () ) ;
  match post_cond.bare with
  | Postcondition.Predicate {name; _} -> Hash_set.add pred_set name
  | _ -> ()

let preds_of vc =
  let pred_set = Identifier.Hash_set.create ~size:4 () in
  preds_of_impl pred_set vc ; Hash_set.to_list pred_set

let preds_of_vcs vcs =
  let pred_set = Identifier.Hash_set.create ~size:16 () in
  List.iter vcs ~f:(preds_of_impl pred_set) ;
  Hash_set.to_list pred_set

let pp fmt {pre_conds; post_cond; _} =
  Fmt.pf fmt "VC(%a, %a)"
    (Fmt.list ~sep:Fmt.comma Precondition.pp)
    (List.rev pre_conds) Postcondition.pp post_cond
