%{
  open Ast
%}

%token
  MSGID "msgid"
  MSGSTR "msgstr"
  MSGCTXT "msgctxt"
  MSGID_PLURAL "msgid_plural"
  LBRACKET "["
  RBRACKET "]"
  EOF
%token <string>
  STRING
  TRANSLATOR "#"
  FLAG "#,"
  EXTRACTED "#."
  REFERENCES "#:" 
  PREVIOUS_CONTEXT "#|"
  OBSOLETE "#~"
%token <int> NUM

%start <[ `Translation of translation | `Comments of comment located list ] list> main
%type <translation> translation

%%

let main :=
  | ~ = nonempty_list(
    | ~ = comments; <`Comments>
    | ~ = translation; <`Translation>  
    ); EOF; <>

// A block of comments that may or may not precede translation data
let comments == located(comment)+

// Translation data
let translation ==
  msgctxt = msgctxt?;
  msgid = msgid;
  (msgstr, msgid_plural, msgstr_plurals) = entry_data; {
    { 
      msgctxt;
      msgid;
      msgstr;
      msgid_plural;
      msgstr_plurals;
    }
  }

// What follows msgid may have two forms: 
//  - Just msgstr with the translated string;
//  - The English plural form of msgid introduced by msgid_plural,
//    followed by an array of translated strings for each plural 
//    form of the target language.
let entry_data ==
  | msgstr = msgstr; { Some msgstr, None, None }
  | msgid_plural = msgid_plural; msgstr_plurals = msgstr_plurals+; { 
      None, Some msgid_plural, Some msgstr_plurals;
    }

let msgid           == located(keyed("msgid"))
let msgid_plural    == located(keyed("msgid_plural"))
let msgstr          == located(keyed("msgstr"))
let msgctxt         == located(keyed("msgctxt"))
let msgstr_plurals  == located("msgstr"; "["; NUM; "]"; ss = STRING+; {
  String.concat "" ss 
}) (* TODO: check if NUM's value is valid *)

let comment :=
  | ~ = "#";  <Translator>
  | ~ = "#."; <Extracted>
  | ~ = "#:"; <Reference>
  | l = "#,"; { Flags (String.split_on_char ',' l) }
  | ~ = "#~"; <Obsolete>
  | ~ = "#|"; <Previous_context>

let keyed(KEY) ==
  KEY; ss = STRING+; { String.concat "" ss }

let located(x) ==
  ~ = x; { { range = $loc; value = x } }
