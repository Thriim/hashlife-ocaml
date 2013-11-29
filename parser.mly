%{
  open Rle_action

 let size = ref 0

 let size s =
   let n = Hmatrix.log2 s in
   if 1 lsl n = s then n else n + 1

%}
/* Tokens */

%token <int> X
%token <int> Y
%token <int> ON
%token <int> OFF
%token RULE
%token EOD
%token EOF
%token <int> NEWLINE

%start file

%type <int * Rle_action.action list> file

%%

file:
| header desc EOF { ($1, List.rev $2) }
;

header:
| X Y RULE { size (max $1 $2) }
;

desc:
| desc step { $2 :: $1 }
| step { [$1] }
;

step:
| ON { On $1 }
| OFF { Off $1 }
| NEWLINE { Newline $1 }
;
