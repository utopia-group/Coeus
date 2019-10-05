{
open RuleParser

exception LexingError of string
}

let DIGIT = ['0'-'9']
let DIGITSEQ = (DIGIT)+

rule lex = parse
  | eof             { T_EOF }
  | "swap"          { T_SWAP }
  | "contra"        { T_CONTRA }
  | "seq"           { T_SEQ }
  | "autoseq"       { T_AUTOSEQ }
  | "blastseq"      { T_BLASTSEQ }
  | "sync"          { T_SYNC }
  | "syncif"        { T_SYNCIF }
  | "rsyncif"       { T_RSYNCIF }
  | "synccall"      { T_SYNCCALL }
  | "psync"         { T_PSYNC }
  | "unroll"        { T_UNROLL }
  | "commute"       { T_COMMUTE }
  | "commute1"      { T_COMMUTE1 }
  | "concat"        { T_CONCAT }
  | "fuse"          { T_FUSE }
  | "sfuse"         { T_SFUSE }
  | "licmup"        { T_LICMUP }
  | "peel"          { T_PEEL }
  | "extend"        { T_EXTEND }
  | "interchange"   { T_INTERCHANGE }
  | "reverse"       { T_REVERSE }
  | "loopelim1"     { T_LOOPELIM1 }
  | "looptorec"     { T_LOOPTOREC }
  | "inline"        { T_INLINE }
  | "_"             { T_SEP }
  | "%"             { T_PSEP }
  | "l"             { T_LEFT }
  | "r"             { T_RIGHT }
  | "n"             { T_NEITHER }
  | DIGITSEQ        {
    let value = int_of_string (Lexing.lexeme lexbuf) in
    T_NUMBER value
  }
  | _  {
    let msg = Fmt.strf "Unrecognized token: %s" (Lexing.lexeme lexbuf) in
    raise (LexingError msg)
  }