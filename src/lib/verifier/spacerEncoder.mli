val preprocess : VerifState.t -> VerifState.t

val encode_sexp_directly :
  VerifState.t -> Core.Sexp.t list * Core.Sexp.t list * Core.Sexp.t

val encode_sexp :
  VerifState.t -> Core.Sexp.t list * Core.Sexp.t list * Core.Sexp.t
