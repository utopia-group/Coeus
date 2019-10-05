open Core

type t = {fix_type: bool; simplify_arith: bool; lift_assume: bool; print: bool}
[@@deriving sexp, compare, hash]

let create_default () =
  {fix_type= false; simplify_arith= false; lift_assume= false; print= false}
