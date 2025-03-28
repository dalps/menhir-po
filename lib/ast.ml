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
(** A value annotated with start and end positions in a source file. *)

type raw_entry = {
  (* We assume every comment corresponds to an entry *)
  is_fuzzy : bool;
  comments : comment located list;
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

type entry = raw_entry located
(** An entry with a location. *)

type po_file = entry list

let is_fuzzy =
  List.exists
    (fun cmt ->
      match cmt.value with Flags flags -> List.mem "fuzzy" flags | _ -> false)
