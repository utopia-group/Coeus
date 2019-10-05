open Archi
open BinNums
open BinPos
open Coqlib
open Datatypes
open Fappli_IEEE
open Fappli_IEEE_bits
open Fappli_IEEE_extra
open Fcore_Zaux
open Integers

type float = binary64

type float32 = binary32

val cmp_of_comparison : comparison -> Datatypes.comparison option -> bool

module Float :
 sig
  val transform_quiet_pl : nan_pl -> nan_pl

  val expand_pl : nan_pl -> nan_pl

  val of_single_pl : bool -> nan_pl -> bool * nan_pl

  val reduce_pl : nan_pl -> nan_pl

  val to_single_pl : bool -> nan_pl -> bool * nan_pl

  val neg_pl : bool -> nan_pl -> bool * nan_pl

  val abs_pl : bool -> nan_pl -> bool * nan_pl

  val binop_pl : binary64 -> binary64 -> bool * nan_pl

  val zero : float

  val eq_dec : float -> float -> bool

  val neg : float -> float

  val abs : float -> float

  val add : float -> float -> float

  val sub : float -> float -> float

  val mul : float -> float -> float

  val div : float -> float -> float

  val cmp : comparison -> float -> float -> bool

  val of_single : float32 -> float

  val to_single : float -> float32

  val to_int : float -> Int.int option

  val to_intu : float -> Int.int option

  val to_long : float -> Int64.int option

  val to_longu : float -> Int64.int option

  val of_int : Int.int -> float

  val of_intu : Int.int -> float

  val of_long : Int64.int -> float

  val of_longu : Int64.int -> float

  val from_parsed : positive -> positive -> coq_Z -> float

  val to_bits : float -> Int64.int

  val of_bits : Int64.int -> float

  val exact_inverse : float -> float option

  val ox8000_0000 : Int.int
 end

module Float32 :
 sig
  val transform_quiet_pl : nan_pl -> nan_pl

  val neg_pl : bool -> nan_pl -> bool * nan_pl

  val abs_pl : bool -> nan_pl -> bool * nan_pl

  val binop_pl : binary32 -> binary32 -> bool * nan_pl

  val zero : float32

  val eq_dec : float32 -> float32 -> bool

  val neg : float32 -> float32

  val abs : float32 -> float32

  val add : float32 -> float32 -> float32

  val sub : float32 -> float32 -> float32

  val mul : float32 -> float32 -> float32

  val div : float32 -> float32 -> float32

  val cmp : comparison -> float32 -> float32 -> bool

  val to_int : float32 -> Int.int option

  val to_intu : float32 -> Int.int option

  val to_long : float32 -> Int64.int option

  val to_longu : float32 -> Int64.int option

  val of_int : Int.int -> float32

  val of_intu : Int.int -> float32

  val of_long : Int64.int -> float32

  val of_longu : Int64.int -> float32

  val from_parsed : positive -> positive -> coq_Z -> float32

  val to_bits : float32 -> Int.int

  val of_bits : Int.int -> float32

  val exact_inverse : float32 -> float32 option
 end
