open BinNums
open Datatypes
open Fappli_IEEE

(** val ptr64 : bool **)

let ptr64 =
  true

(** val big_endian : bool **)

let big_endian =
  false

(** val align_int64 : coq_Z **)

let align_int64 =
  Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val align_float64 : coq_Z **)

let align_float64 =
  Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val splitlong : bool **)

let splitlong =
  negb ptr64

(** val default_pl_64 : bool * nan_pl **)

let default_pl_64 =
  (true,
    (let rec f = function
     | O -> Coq_xH
     | S n0 -> Coq_xO (f n0)
     in f (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S
          O)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val choose_binop_pl_64 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_64 _ _ _ _ =
  false

(** val default_pl_32 : bool * nan_pl **)

let default_pl_32 =
  (true,
    (let rec f = function
     | O -> Coq_xH
     | S n0 -> Coq_xO (f n0)
     in f (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          O))))))))))))))))))))))))

(** val choose_binop_pl_32 : bool -> nan_pl -> bool -> nan_pl -> bool **)

let choose_binop_pl_32 _ _ _ _ =
  false

(** val float_of_single_preserves_sNaN : bool **)

let float_of_single_preserves_sNaN =
  false
