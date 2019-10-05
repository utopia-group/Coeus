open Core

type t =
  { disable_houdini: bool
  ; disable_spacer: bool
  ; disable_unsat_core: bool
  ; houdini_timeout: Core.Time.Span.t
  ; houdini_candidate_limit: int
  ; spacer_timeout: Core.Time.Span.t
  ; unsat_core_timeout: Core.Time.Span.t
  ; keep_tmp_file: bool }

let pp fmt
    { disable_houdini
    ; disable_spacer
    ; disable_unsat_core
    ; houdini_timeout
    ; houdini_candidate_limit
    ; spacer_timeout
    ; unsat_core_timeout
    ; keep_tmp_file } =
  let houdini_time_in_ms = Time.Span.to_ms houdini_timeout in
  let spacer_time_in_ms = Time.Span.to_ms spacer_timeout in
  let unsat_core_time_in_ms = Time.Span.to_ms unsat_core_timeout in
  let print_enable fmt b =
    if b then Fmt.pf fmt "enabled" else Fmt.pf fmt "disabled"
  in
  Fmt.pf fmt
    "houdini: %a; houdini_timeout(ms): %f; houdini_candidate_limit: %d; \
     spacer: %a; spacer_timeout(ms): %f; unsat_core: %a; \
     unsat_core_timeout(ms): %f; keep_tmp_file: %b"
    print_enable (not disable_houdini) houdini_time_in_ms
    houdini_candidate_limit print_enable (not disable_spacer) spacer_time_in_ms
    print_enable (not disable_unsat_core) unsat_core_time_in_ms keep_tmp_file
