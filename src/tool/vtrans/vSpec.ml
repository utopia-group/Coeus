open Core
open Ast.Ecoeus

module VExpr = struct
  type t =
    | IntLiteral of int
    | Var of string
    | Plus of t * t
    | Minus of t * t
    | Mult of t * t
    | Div of t * t
    | Mod of t * t
    | Eq of t * t
    | Lt of t * t
    | Le of t * t
    | Gt of t * t
    | Ge of t * t
    | ArrayRead of {base: string; bound: string; index: string; value: string}
    | Val of {prog_var: string; spec_var: string}

  let rec pp fmt e =
    let pp_infix op es =
      Fmt.pf fmt "(%a)"
        (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " %s " op) pp)
        es
    in
    let pp_prefix op es =
      Fmt.pf fmt "%s%a" op (Fmt.parens (Fmt.list ~sep:Fmt.comma pp)) es
    in
    match e with
    | IntLiteral i -> Fmt.int fmt i
    | Var s -> Fmt.string fmt s
    | Plus (lhs, rhs) -> pp_infix "+" [lhs; rhs]
    | Minus (lhs, rhs) -> pp_infix "-" [lhs; rhs]
    | Mult (lhs, rhs) -> pp_infix "*" [lhs; rhs]
    | Div (lhs, rhs) -> pp_infix "/" [lhs; rhs]
    | Mod (lhs, rhs) -> pp_prefix "mod" [lhs; rhs]
    | Eq (lhs, rhs) -> pp_infix "=:=" [lhs; rhs]
    | Lt (lhs, rhs) -> pp_infix "<" [lhs; rhs]
    | Le (lhs, rhs) -> pp_infix "=<" [lhs; rhs]
    | Gt (lhs, rhs) -> pp_infix ">" [lhs; rhs]
    | Ge (lhs, rhs) -> pp_infix ">=" [lhs; rhs]
    | Val {prog_var; spec_var} -> Fmt.pf fmt "val(%s, %s)" prog_var spec_var
    | ArrayRead {base; bound; index; value} ->
        Fmt.pf fmt "read((%s, %s), %s, %s)" base bound index value

  let pp_oneline fmt e = Fmt.pf fmt "@[<h>%a@]" pp e
end

module VPred = struct
  module Kind = struct
    type t = NewLeft | NewRight | Val

    let pp fmt = function
      | NewLeft -> Fmt.string fmt "new11"
      | NewRight -> Fmt.string fmt "new21"
      | Val -> Fmt.string fmt "val"
  end

  type t = {kind: Kind.t; params: string list}

  let pp fmt {kind; params} =
    Fmt.pf fmt "%a%a" Kind.pp kind
      (Fmt.parens (Fmt.list ~sep:Fmt.comma Fmt.string))
      params

  let pp_oneline fmt pred = Fmt.pf fmt "@[<h>%a@]" pp pred
end

type t = {bindings: VPred.t list; conds: VExpr.t list}

let pp fmt {bindings; conds} =
  Fmt.pf fmt "@[<hov 2>incorrect :-@ " ;
  Fmt.pf fmt "%a" (Fmt.list ~sep:Fmt.comma VPred.pp_oneline) bindings ;
  if not (List.is_empty bindings) then Fmt.pf fmt "%a" Fmt.comma () ;
  Fmt.pf fmt "%a" (Fmt.list ~sep:Fmt.comma VExpr.pp_oneline) conds ;
  Fmt.pf fmt ".@]"
