type comment =
  | Translator of string
  | Extracted of string
  | Reference of string
  | Flags of string list
  | Obsolete of string
  | Previous_context of string
  | Previous_untranslated_string of string
  | Previous_untranslated_string_plural of string

type 'a located = { range : Lexing.(position * position); value : 'a }
(** A value annotated with start and end positions in a source file. *)

type translation = {
  msgid : string located;
  msgid_plural : string located option;
  msgctxt : string located option;
  msgstr : string located option;
  msgstr_plurals : string located list option;
}
(** Entry fields, decorated with locations. All fields except msgid are
    optional. The valid combinations are updheld by the grammar under the
    following invariants:
    - The presence of a context string is orthogonal to the presence of plural
      forms;
    - The msgid_plural, msgstr_plural pair and msgstr are mutually exclusive. *)


type po = {
  messages: translation located list;
  obsolete: comment located list;
}
