{
  open Parser

  exception LexError of string
}

let white = [' ''\t']+
let newline = "\n" | "\r" | "\n\r"
let id = ['a'-'z''A'-'Z']

rule next_token =
  parse
  | "msgid"   { MSGID }
  | "msgstr"  { print_endline "found msgstr"; MSGSTR }
  | '#' _? white* ([^'#''\n']* as content)     { Printf.printf "found comment: %s\n" content; COMMENT content }
  | '\"'([^'\"']* as content)'\"'     { TEXT content }
  | white      { next_token lexbuf }
  | newline    { Lexing.new_line lexbuf; next_token lexbuf }
  | eof        { EOF }
  | _ as c { raise (LexError (Printf.sprintf "[lexer] %s, line %d, character %d: unexpected character '%c'" lexbuf.lex_start_p.pos_fname lexbuf.lex_start_p.pos_lnum (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol) c)) }