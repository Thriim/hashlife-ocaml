
%{

  open Mcop_ast

%}

%token <string> IDENT FILENAME
%token <int> NUMBER

%token LET IN UNION INTER DIFF MIRROR ROT90 ROT180 ROT270 RXTD LXTD
%token READRLE READMC OUTRLE OUTMC ITER EXTEND
%token COMMA LP RP EQUALS EOF

%start file
%type <Mcop_ast.file> file

%%

file:
| decl_list statement { (List.rev $1, $2) }
;

decl_list:
| /* epsilon */ { [] }
| decl_list decl { $2::$1 }
;

decl:
| LET IDENT EQUALS expr IN
    { ($2, $4) }
;

statement:
| OUTMC LP expr COMMA FILENAME RP
    { OutMc($3, $5) }
| OUTRLE LP expr COMMA FILENAME RP
    { OutRle($3, $5) }
| expr
    { Draw($1) }
;

expr:
| IDENT
    { EVar($1) }
| UNION LP expr COMMA expr RP
    { EUnion($3, $5) }
| INTER LP expr COMMA expr RP
    { EInter($3, $5) }
| DIFF LP expr COMMA expr RP
    { EDiff($3, $5) }
| READRLE LP FILENAME RP
    { EReadRle($3) }
| READMC LP FILENAME RP
    { EReadMc($3) }
| ITER LP expr COMMA NUMBER RP
    { EIter($3, $5) }
| MIRROR LP expr RP
    { EMirror($3) }
| ROT90 LP expr RP
    { ERot90($3) }
| ROT180 LP expr RP
    { ERot180($3) }
| ROT270 LP expr RP
    { ERot270($3) }
| EXTEND LP expr RP
    { EExtend($3) }
| LXTD LP expr RP
    { ELxtd($3) }
| RXTD LP expr RP
    { ERxtd($3) }
;

