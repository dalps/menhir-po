{
  open Parser

  exception LexError of string

  let illegal c (lexbuf : Lexing.lexbuf) =
    raise
      (LexError
        (Printf.sprintf
            "[lexer] %s, line %d, character %d: unexpected character '%c'"
            lexbuf.lex_start_p.pos_fname lexbuf.lex_start_p.pos_lnum
            (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
            c))
}

let white = [' ''\t']+
let newline = "\n" | "\r" | "\n\r"
let id = ['a'-'z''A'-'Z']

(* Comments start at the beginning of a line with ‘#’ and extend until the
end of the PO file line. *)

rule next_token =
  parse
  | "msgid"         { MSGID }
  | "msgstr"        { MSGSTR }
  | "msgctxt"       { MSGCTXT }
  | "msgid_plural"  { MSGID_PLURAL }
  | "msgstr_plural" { MSGSTR_PLURAL }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | ','             { COMMA }
  | '#'             { TRANSLATOR_COMMENT }
  | "#."            { EXTRACTED_COMMENT }
  | "#:"            { REFERENCES }
  | "#,"            { FLAGS }
  | "#|"            { PREVIOUS_CONTEXT }
  | "#~"            { OBSOLETE_MESSAGE }
  | [0-9]+ as n     { NUM n }
  | '\"'([^'\"']* as content)'\"'       { STRING content }
  | [^'\n']* as comment_content         { COMMENT comment_content }
  | white           { next_token lexbuf }
  | newline         { Lexing.new_line lexbuf; next_token lexbuf }
  | eof             { EOF }
  | _ as c { illegal c lexbuf }
