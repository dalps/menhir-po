%{
  open Ast
%}

%token
  MSGID               "msgid"
  MSGSTR              "msgstr"
  MSGCTXT             "msgctxt"
  MSGID_PLURAL        "msgid_plural"
  COMMA               ","
  LBRACKET            "["
  RBRACKET            "]"
  EOF
%token <string>
  TRANSLATOR_COMMENT  "#"
  FLAG                "#,"
  EXTRACTED_COMMENT   "#."
  REFERENCES          "#:" 
  PREVIOUS_CONTEXT  "#|"
  OBSOLETE_MESSAGE    "#~"
%token <int> NUM
%token <string> STRING

%start <po_file> main
%type <entry> entry

%%

let main :=
  | ~ = list(entry); EOF; <>

let entry ==
  located(
  | singular_entry
  | plural_entry
  )

let msgid         == located(keyed("msgid"))
let msgid_plural  == located(keyed("msgid_plural"))
let msgstr        == located(keyed("msgstr"))
let msgctxt       == located(keyed("msgctxt"))
let msgstr_plural == located("msgstr"; "["; NUM; "]"; ss = STRING+; { String.concat "" ss }) (* TODO: check if NUM's value is valid *)

let singular_entry ==
  comments = located(comment)+;
  msgctxt = msgctxt?;
  msgid = msgid;
  msgstr = msgstr; {
    {
      comments;
      msgctxt;
      msgid;
      msgstr = Some msgstr;
      msgid_plural = None;
      msgstr_plural = None;
    }
  }

let plural_entry ==
  comments = located(comment)+;
  msgctxt = msgctxt?;
  msgid = msgid;
  msgid_plural = msgid_plural;
  msgstr_plural = msgstr_plural+; {
    {
      comments;
      msgctxt;
      msgid;
      msgstr = None;
      msgid_plural = Some msgid_plural;
      msgstr_plural = Some msgstr_plural;
    }
  }

let comment :=
  | ~ = "#";  <Translator>
  | ~ = "#."; <Extracted>
  | ~ = "#:"; <Reference>
  | l = "#,"; { Flags (String.split_on_char ',' l) }
  | ~ = "#~"; <Obsolete_message>
  | ~ = "#|"; <Previous_context>

let keyed(KEY) ==
  KEY; ss = STRING+; { String.concat "" ss }

let located(x) ==
  ~ = x; { { range = $loc; value = x } }