%{
  open Ast
%}

%token MSGID MSGSTR QUOTE EOF
%token <string> COMMENT
%token <string> TEXT

%start <entry list> main

%%

let main :=
  | messages = list(message); EOF; { messages }

let message :=
  comments = COMMENT*;
  MSGID; msgid = TEXT+;
  MSGSTR; msgstr = TEXT+; {
    print_endline "ok!";
    Message {
      comments;
      msgid = String.concat "\n" msgid;
      msgstr = String.concat "\n" msgstr;
    }
  }