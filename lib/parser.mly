%{
  open Ast
%}

%token
  MSGID               "msgid"
  MSGSTR              "msgstr"
  MSGCTXT             "msgctxt"
  MSGID_PLURAL        "msgid_plural"
  LBRACKET            "["
  RBRACKET            "]"
  EOF
%token <string>
  TRANSLATOR_COMMENT  "#"
  FLAG                "#,"
  EXTRACTED_COMMENT   "#."
  REFERENCES          "#:" 
  PREVIOUS_CONTEXT    "#|"
  OBSOLETE_MESSAGE    "#~"
%token <int> NUM
%token <string> STRING

%start <po_file> main
%type <entry> entry

%%

let main :=
  | ~ = list(entry); EOF; <>

let entry ==
  ~ = located(
    comments = located(comment)+;
    msgctxt = msgctxt?;
    msgid = msgid;
    (msgstr, msgid_plural, msgstr_plural) = entry_data; {
      { 
        comments;
        msgctxt;
        msgid;
        msgstr;
        msgid_plural;
        msgstr_plural;
      }
    }
  ); <>

(* What follows msgid may have two forms: 
  - Just msgstr with the translated string;
  - The English plural form of msgid introduced by msgid_plural,
    followed by an array of translated strings for each plural form of the target language.
*)
let entry_data ==
  | msgstr = msgstr; { Some msgstr, None, None }
  | msgid_plural = msgid_plural;
    msgstr_plural = msgstr_plural+; { 
    None, Some msgid_plural, Some msgstr_plural;
  }

let msgid         == located(keyed("msgid"))
let msgid_plural  == located(keyed("msgid_plural"))
let msgstr        == located(keyed("msgstr"))
let msgctxt       == located(keyed("msgctxt"))
let msgstr_plural == located("msgstr"; "["; NUM; "]"; ss = STRING+; { String.concat "" ss }) (* TODO: check if NUM's value is valid *)

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
