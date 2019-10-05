(* Create a prompt based on the current interpreter state *)
let make_prompt size ReplState.({session_state; _}) =
  let open LTerm_text in
  let open LTerm_style in
  let code =
    Core.Option.value_map session_state ~default:"" ~f:
      (fun SessionState.({history; _}) ->
        match history with
        | [] -> "remaining goals: ?"
        | SessionState.HistoryState.{ prover_state = Prover.ProverState.({goals; _}); _} :: _ ->
            Printf.sprintf "remaining goals: %d" (List.length goals) )
  in
  (* Shorten the path if it is too large for the size of the
     terminal. *)
  let path =
    Core.Option.value_map session_state ~default:"no program loaded" ~f:
      (fun SessionState.({file_path; _}) -> file_path )
  in
  let path_len = Zed_utf8.length path in
  let size_for_path = size.LTerm_geom.cols - 12 - Zed_utf8.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Zed_utf8.after path (path_len - size_for_path + 2)
      else path
    else path
  in
  eval
    [ B_bold true
    ; B_fg lcyan
    ; S "─< "
    ; B_fg lyellow
    ; S path
    ; E_fg
    ; S " >─"
    ; S
        (Zed_utf8.make
           ( size.LTerm_geom.cols - 12 - Zed_utf8.length code
           - Zed_utf8.length path )
           (CamomileLibrary.UChar.of_int 0x2500))
    ; S "[ "
    ; B_fg lwhite
    ; S code
    ; E_fg
    ; S " ]─"
    ; E_fg
    ; S "\n"
    ; B_fg lred
    ; S (try Sys.getenv "USER" with Not_found -> "")
    ; E_fg
    ; B_fg lgreen
    ; S "@"
    ; E_fg
    ; B_fg lblue
    ; S "coeus-repl"
    ; E_fg
    ; B_fg lgreen
    ; S " "
    ; S (Zed_utf8.singleton (CamomileLibrary.UChar.of_int 0x03BB))
    ; S " "
    ; E_fg
    ; E_bold ]


(* Format the interpreter output for REPL display *)
let make_output out =
  let open LTerm_text in
  let output = Printf.sprintf "%s" out in
  eval [S output]


(* Customization of the read-line engine  *)
class read_line ~term ~history ~state = object(self)

  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (React.S.l1 (fun size -> make_prompt size state) self#size)
end

let rec loop term history (curr_state: ReplState.t) =
  let open Lwt in
  catch
    (fun () ->
      let rl =
        new read_line
          ~term
          ~history:(LTerm_history.contents history)
          ~state:curr_state
      in
      rl#run >|= fun command -> Some command )
    (function Sys.Break -> return None | exn -> Lwt.fail exn)
  >>= function
    | Some command ->
        let state, out = ReplEvaluator.eval curr_state command in
        LTerm.fprintls term (make_output out)
        >>= fun () ->
        LTerm_history.add history command ;
        loop term history state
    | None -> loop term history curr_state


let main init_state =
  Lwt.catch
    (fun () ->
      let open Lwt in
      Lazy.force LTerm.stdout
      >>= fun term -> loop term (LTerm_history.create []) init_state )
    (function
        | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)


let loop init_state = Lwt_main.run (main init_state)
