open Core

module Status = struct
  type t = Finished of string | Timeout
end

let run_with_timeout ~f timeout_span =
  let timeout_sec = Time.Span.to_sec timeout_span |> int_of_float in
  let reader, writer = Unix.pipe () in
  (* Forking a new process is the easiest way to perform an operation with a timeout *)
  (* Also, Z3 occassionally crashes and we don't want to use it in-process as doing it may bring down our prover as well *)
  (* This trick is taken from https://github.com/c-cube/ocaml-containers/issues/143 *)
  match Unix.fork () with
  | `In_the_child ->
      Unix.close reader ;
      let _ = Unix.alarm timeout_sec in
      let oc = Unix.out_channel_of_descr writer in
      let msg = f () in
      Out_channel.output_string oc msg ;
      Out_channel.flush oc ;
      Unix.close writer ;
      Unix.exit_immediately 0
  | `In_the_parent pid ->
      Unix.close writer ;
      let do_run () =
        match Unix.waitpid pid with
        | Result.Error (`Signal s) when s = Signal.alrm ->
            Result.Ok Status.Timeout
        | Result.Error (`Signal s) ->
            let msg =
              Fmt.strf "Subprocess exits with unexpected signal \"%s\""
                (Signal.to_string s)
            in
            Result.Error msg
        | Result.Error (`Exit_non_zero exit_code) ->
            let msg =
              Fmt.strf "Subprocess exits with unexpected return code \"%d\""
                exit_code
            in
            Result.Error msg
        | Result.Ok () ->
            let ic = Unix.in_channel_of_descr reader in
            let msg = In_channel.input_all ic in
            Result.Ok (Status.Finished msg)
      in
      let result = do_run () in
      Unix.close reader ; result
