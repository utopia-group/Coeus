open Core
open Ast.Ecoeus

type t = {name: Identifier.t; params: VarBinding.t list}
[@@deriving sexp, compare, hash]

let param_types_of {params; _} =
  List.map params ~f:(fun VarBinding.({ty; _}) -> ty)

let smtlib_of_t {name; params} =
  let param_sexp =
    Sexp.List
      (List.map params ~f:(fun VarBinding.({ty; _}) -> Type.sexp_of_t ty))
  in
  Sexp.List
    [ Sexp.Atom "declare-fun"
    ; Identifier.sexp_of_t name
    ; param_sexp
    ; Type.sexp_of_t Type.BoolType ]

let z3rel_of_t {name; params} =
  let param_sexp =
    Sexp.List
      (List.map params ~f:(fun VarBinding.({ty; _}) -> Type.sexp_of_t ty))
  in
  Sexp.List [Sexp.Atom "declare-rel"; Identifier.sexp_of_t name; param_sexp]

let t_of_smtlib = function
  | Sexp.List
      [ Sexp.Atom "declare-fun"
      ; name_sexp
      ; Sexp.List param_sexps
      ; Sexp.Atom "Bool" ] ->
      let name = Identifier.t_of_sexp name_sexp in
      let params =
        List.mapi param_sexps ~f:(fun idx sexp ->
            let name = Identifier.of_string (Fmt.strf "v%d" idx) in
            let ty = Type.t_of_sexp sexp in
            VarBinding.{name; ty} )
      in
      {name; params}
  | _ as sexp ->
      let msg = "Unrecognized sexp for predicate declaration" in
      raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

let t_of_z3rel = function
  | Sexp.List [Sexp.Atom "declare-rel"; name_sexp; Sexp.List param_sexps] ->
      let name = Identifier.t_of_sexp name_sexp in
      let params =
        List.mapi param_sexps ~f:(fun idx sexp ->
            let name = Identifier.of_string (Fmt.strf "v%d" idx) in
            let ty = Type.t_of_sexp sexp in
            VarBinding.{name; ty} )
      in
      {name; params}
  | _ as sexp ->
      let msg = "Unrecognized sexp for predicate declaration" in
      raise (Sexplib.Conv.Of_sexp_error (Failure msg, sexp))

let z3_query_id = Identifier.of_string "$END_QUERY"

let z3_query_rel = {name= z3_query_id; params= []}

let summary_pre_id fun_name =
  let name = Fmt.strf "$PRE_%a" Identifier.pp fun_name in
  Identifier.of_string name

let summary_post_id fun_name =
  let name = Fmt.strf "$POST_%a" Identifier.pp fun_name in
  Identifier.of_string name

let mutual_summary_pre_id fun_name0 fun_name1 =
  let name =
    Fmt.strf "$REL_PRE_%a_%a" Identifier.pp fun_name0 Identifier.pp fun_name1
  in
  Identifier.of_string name

let mutual_summary_post_id fun_name0 fun_name1 =
  let name =
    Fmt.strf "$REL_POST_%a_%a" Identifier.pp fun_name0 Identifier.pp fun_name1
  in
  Identifier.of_string name

let pp fmt {name; params} =
  Fmt.pf fmt "%a(%a)" Identifier.pp name
    (Fmt.list ~sep:Fmt.comma VarBinding.pp)
    params
