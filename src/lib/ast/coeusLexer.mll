{
open CoeusParser
open Core

exception LexerError of string
}

let NEWLINE = ("\r"|"\n"|"\r\n")
let WHITESPACE_INLINE_OPT = [' ' '\t']*
let WHITESPACE_INLINE = [' ' '\t']+
let WHITESPACE = [' ' '\r' '\n' '\t']+

let DIGIT = ['0'-'9']
let LETTER = ['a'-'z''A'-'Z']
let SPECIAL = ['.' '_' '\'']
let NONDIGIT = (LETTER|SPECIAL)
let NAME = NONDIGIT(NONDIGIT|DIGIT)*

let DIGITSEQ = (DIGIT)+
let BINDIGIT = ['0'-'1']
let OCTDIGIT = ['0'-'7']
let HEXDIGIT = ['0'-'9' 'a'-'f' 'A'-'F']
let BINPREFIX = "0b"
let HEXPREFIX = "0x"
let OCTPREFIX = "0o"
let BINLITERAL = BINPREFIX(BINDIGIT)+
let HEXLITERAL = HEXPREFIX(HEXDIGIT)+
let OCTLITERAL = OCTPREFIX(OCTDIGIT)+
let DECLITERAL = DIGITSEQ

rule input_file state = parse
  | WHITESPACE_INLINE { input_file state lexbuf }
  | NEWLINE { Lexing.new_line lexbuf; input_file state lexbuf }
  | eof  { T_EOF }

  (* Handle comment *)
  | "//" { line_comment state lexbuf }
  | "/*" { delim_comment state lexbuf }

  (* Handle numerics *)
  | BINLITERAL | HEXLITERAL | OCTLITERAL | DECLITERAL {
    T_NUMBER (Bigint.of_string (Lexing.lexeme lexbuf))
  }
  
  (* Handle parens *)
  | "("  { T_LP }
  | ")"  { T_RP }
  | "["  { T_LB }
  | "]"  { T_RB }
  | "{"  { T_LC }
  | "}"  { T_RC }

  (* Handle operators *)
  | "+"     { T_PLUS }
  | "++"    { T_PLUSPLUS }
  | "+="    { T_PLUSEQ }
  | "-"     { T_MINUS }
  | "--"    { T_MINUSMINUS }
  | "-="    { T_MINUSEQ }
  | "*"     { T_TIMES }
  | "*="    { T_TIMESEQ }
  | "/"     { T_DIV }
  | "/="    { T_DIVEQ }
  | "%"     { T_MOD }
  | "%="    { T_MODEQ }
  | "&&"    { T_AND }
  | "||"    { T_OR }
  | "==>"   { T_IMPLIES }
  | "=="    { T_EQ }
  | "!="    { T_NE }
  | "<"     { T_LT }
  | "<="    { T_LE }
  | ">"     { T_GT }
  | ">="    { T_GE }
  | "!"     { T_NOT }
  | ":"     { T_COLON }
  | "::"    { T_DCOLON }
  | "="     { T_ASSIGN }
  | ";"     { T_SEMI }
  | ","     { T_COMMA }

  (* Handle keywords *)
  | "if"         { T_IF }
  | "then"       { T_THEN }
  | "else"       { T_ELSE }
  | "while"      { T_WHILE }
  | "for"        { T_FOR }
  | "to"         { T_TO }
  | "downto"     { T_DOWNTO }
  | "step"       { T_STEP }
  | "call"       { T_CALL }
  | "assume"     { T_ASSUME }
  | "true"       { T_TRUE }
  | "false"      { T_FALSE }
  | "int"        { T_INT }
  | "bool"       { T_BOOL }
  | "returns"    { T_RETURNS }
  | "procedure"  { T_PROCEDURE }
  | "declare"    { T_DECLARE }
  | "requires"   { T_REQUIRES }
  | "ensures"    { T_ENSURES }
  | "forall"     { T_FORALL }
  | "exists"     { T_EXISTS }
  | "$lentry"    { T_LEFT_ENTRY }
  | "$rentry"    { T_RIGHT_ENTRY }
  | "$L"         { T_LEFT_ANNOT }
  | "$R"         { T_RIGHT_ANNOT }

  (* Handle variable *)
  | NAME  { T_IDENT (Coeus.Identifier.of_string (Lexing.lexeme lexbuf)) }
  | _     { let msg = Printf.sprintf "Unrecognized token: %s" (Lexing.lexeme lexbuf) in raise (LexerError msg) }

and line_comment state = parse
  | NEWLINE { Lexing.new_line lexbuf; input_file state lexbuf }
  | _       { line_comment state lexbuf }

and delim_comment state = parse
  | NEWLINE { Lexing.new_line lexbuf; delim_comment state lexbuf }
  | "*/" { input_file state lexbuf }
  | _    { delim_comment state lexbuf }

{

let set_lexbuf_filename buf name = 
  let open Lexing in
  let pos = buf.lex_curr_p in
    buf.lex_curr_p <-
    { pos with pos_fname = name
    }

let get_curr_pos buf =
  let open Lexing in
  let pos = buf.lex_curr_p in
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

}