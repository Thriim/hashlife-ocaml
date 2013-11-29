%{
  open Rle_action

 let size = ref 0

 let size s =
   let n = Hmatrix.log2 s in
   if 1 lsl n = s then n else n + 1

%}
/* Tokens */

%token ON
%token OFF
%token EOF
%token NEWLINE
%token ENDLINE
%token <int> INT

%start file

%type <Rle_action.mcell_desc list> file

%%

file:
| desc EOF { List.rev $1 }
;

desc:
| desc step { $2 :: $1 }
| step { [$1] }
;

step:
| leaf ENDLINE { Leaf (List.rev $1) }
| node ENDLINE { $1 }
;

node:
| INT INT INT INT INT { Node($1, $2, $3, $4, $5) }
;

leaf:
| leaf leaf_step { $2 :: $1 }
| leaf_step { [$1] }
;

leaf_step:
| ON { On 1 }
| OFF { Off 1 }
| NEWLINE { Newline 1 }
;

