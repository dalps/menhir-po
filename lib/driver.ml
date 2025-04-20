include Nice_parser.Make (struct
  type token = Parser.token
  type result = Ast.po_file

  exception ParseError = Parser.Error

  let parse = Parser.main

  include Lexer
end)

let show_token (t : token) =
  match t with
  | Parser.STRING t -> "STRING " ^ t
  | Parser.MSGSTR -> "msgstr"
  | Parser.MSGID -> "msgid"
  | Parser.MSGCTXT -> "msgctxt"
  | Parser.MSGID_PLURAL -> "msgid_plural"
  | Parser.EOF -> "eof"
  | _ -> "other token"

let () =
  pp_exceptions ();
  Location.register_error_of_exn (function
    | LexError { msg; loc } -> Some (Location.error ~loc msg)
    | ParseError { token; loc } ->
        Some
          (Location.error ~loc
             (Printf.sprintf "[parser] unexpected token \"%s\""
                (show_token token)))
    | _ -> None)
