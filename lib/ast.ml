type flag = Fuzzy of bool | Other of string

type comment_kind =
  | Translator of string
  | Extracted of string
  | Reference of string
  | Flag of flag
  | Obsolete_message of string
  | Previous_context of string
  | Previous_untranslated_string of string
  | Previous_untranslated_string_plural of string

type entry =
  | Message of {
      comments : string list;
      (* extracted_comments : string list;
  fuzzy : bool; *)
      msgid : string;
      (* msgid_plural : string; *)
      msgstr : string; (* msgstr_plural : string list; *)
    }
  | Plural_message

type po_file = { author : string; messages : entry list }
