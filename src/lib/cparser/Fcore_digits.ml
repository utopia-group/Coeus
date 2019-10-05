open BinNums
open BinPos

(** val digits2_pos : positive -> positive **)

let rec digits2_pos = function
| Coq_xI p -> Pos.succ (digits2_pos p)
| Coq_xO p -> Pos.succ (digits2_pos p)
| Coq_xH -> Coq_xH

(** val coq_Zdigits2 : coq_Z -> coq_Z **)

let coq_Zdigits2 n = match n with
| Z0 -> n
| Zpos p -> Zpos (digits2_pos p)
| Zneg p -> Zpos (digits2_pos p)
