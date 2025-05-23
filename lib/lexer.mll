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
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | "#." white* ([^'\n']* as content)           { EXTRACTED content }
  | "#:" white* ([^'\n']* as content)           { REFERENCES content }
  | "#," white* ([^'\n']* as content)           { FLAG content }
  | "#|" white* ([^'\n']* as content)           { PREVIOUS_CONTEXT content }
  | "#~" white* ([^'\n']* as content)            { OBSOLETE content }
  | '#'  white* ([^'\n']* as content)           { TRANSLATOR content }
  | ['0'-'9']+ as n     { NUM (int_of_string n) }
  | '\"'([^'\"']* as content)'\"'       { STRING content }
  | white           { next_token lexbuf }
  | newline         { Lexing.new_line lexbuf; next_token lexbuf }
  | eof             { EOF }
  | _ as c { illegal c lexbuf }
