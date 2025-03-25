%{
  open Ast
%}

%token
  MSGID               "msgid"
  MSGSTR              "msgstr"
  MSGCTXT             "msgctxt"
  MSGID_PLURAL        "msgid_plural"
  MSGSTR_PLURAL       "msgstr_plural"
  TRANSLATOR_COMMENT  "#"
  FLAG                "#,"
  EXTRACTED_COMMENT   "#."
  REFERENCES          "#:" 
  PREVIOUS_CONTEXT    "#|"
  OBSOLETE_MESSAGE    "#~"
  COMMA               ","
  LBRACKET            "["
  RBRACKET            "]"
  EOF
%token <int> NUM
%token <string> COMMENT
%token <string> TEXT

%start <entry list> main

%%

let main :=
  | messages = list(message); EOF; { messages }

let message :=
  comments = comment*;
  "msgctxt"; ctxt = TEXT+;
  MSGID; msgid = TEXT+;
  MSGSTR; msgstr = TEXT+; {
    print_endline "ok!";
    Message {
      comments;
      msgid = String.concat "" msgid;
      msgstr = String.concat "" msgstr;
    }
  }

plural_entry:
  "msgid" u = TEXT+
  "msgid_plural"  p = TEXT+
  "msgstr_plural" "[" n = NUM "]" 

  { }

let comment :=
  | "#"; ~ = COMMENT; <Translator>
  | "#."; ~ = COMMENT;  <Extracted>
  | "#:"; ~ = comment;  <Reference>
  | "#,"; flags = separated_list(",", comment); { Flags flags }
  | "#~"; ~ = COMMENT;  <Obsolete_message>
  | "#|"; "msgctxt"; ~ = COMMENT;  <Previous_context>
  | "#|"; "msgid"; ~ = COMMENT;  <Previous_untranslated_string>
  | "#|"; "msgid_plural"; ~ = COMMENT; <Previous_untranslated_string_plural>
