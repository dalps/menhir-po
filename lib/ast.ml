type flag =
| Fuzzy of bool
| C_format of bool option

type comment =
  Translator
  | Extracted
  | Reference
  | Flag of flag

type entry = Message of {
  comments : string list;
  (* extracted_comments : string list;
  fuzzy : bool; *)
  msgid : string;
  (* msgid_plural : string; *)
  msgstr : string;
  (* msgstr_plural : string list; *)
}

type po_file = {
  author : string;
  messages : entry list;
}
