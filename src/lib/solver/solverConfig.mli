type t =
  { (* Solver switches *)
    disable_houdini: bool
  ; disable_spacer: bool
  ; disable_unsat_core: bool
  ; (* Houdini options *)
    houdini_timeout: Core.Time.Span.t
  ; houdini_candidate_limit: int
  ; (* Spacer options *)
    spacer_timeout: Core.Time.Span.t
  ; unsat_core_timeout: Core.Time.Span.t
  ; keep_tmp_file: bool }

val pp : t Fmt.t
