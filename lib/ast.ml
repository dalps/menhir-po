type flag = Fuzzy of bool | Other of string

type comment =
  | Translator of string
  | Extracted of string
  | Reference of string
  | Flags of string list
  | Obsolete_message of string
  | Previous_context of string
  | Previous_untranslated_string of string
  | Previous_untranslated_string_plural of string

type 'a located = { range : Lexing.(position * position); value : 'a }

(* All fields except msgid are optional, although only certain combinations are valid, which are updheld by the grammar.
Namely:
- The presence of context is orthogonal to the presence of plural forms;
- msgid_plural and msgstr_plural always appear together, and in that case msgstr is None. *)
type raw_entry = {
  comments : comment located list;
      (* We assume every comment corresponds to an entry *)
  msgid : string located;
  msgid_plural : string located option;
  msgctxt : string located option;
  msgstr : string located option;
  msgstr_plural : string located list option;
}

type entry = raw_entry located
type po_file = entry list
