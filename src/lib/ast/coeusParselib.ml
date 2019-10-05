open Core

let error_msg ?msg fname lnum cnum =
  let msg =
    Option.value_map msg ~default:"" ~f:(fun s -> Printf.sprintf ": %s" s)
  in
  Printf.sprintf "Syntax error at line %d, column %d in file %s%s" lnum cnum
    fname msg


let do_parse fname lexbuf =
  let lexer = CoeusLexer.input_file () in
  try Result.Ok (CoeusParser.main lexer lexbuf) with
  | CoeusParser.Error ->
      let _, lnum, cnum = CoeusLexer.get_curr_pos lexbuf in
      let msg = error_msg fname lnum cnum in
      Result.Error msg
  | CoeusLexer.LexerError msg | Failure msg ->
      let _, lnum, cnum = CoeusLexer.get_curr_pos lexbuf in
      let msg = error_msg ~msg fname lnum cnum in
      Result.Error msg


let parse_file fname =
  let parse_from_channel name in_channel =
    let lexbuf = Lexing.from_channel in_channel in
    do_parse name lexbuf
  in
  if String.equal fname "-" then parse_from_channel "<stdin>" In_channel.stdin
  else
    match Sys.file_exists ~follow_symlinks:true fname with
    | `Yes ->
        In_channel.with_file ~binary:false fname ~f:(fun channel ->
            parse_from_channel fname channel )
    | _ ->
        let msg = Fmt.strf "No such file or directory: %s" fname in
        Result.Error msg


let parse_string str =
  let lexbuf = Lexing.from_string str in
  do_parse "<string>" lexbuf
