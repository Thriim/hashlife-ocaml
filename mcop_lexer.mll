
{
  open Mcop_parser

  exception Lexical_error of string

  let str_buff = Buffer.create 512

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ "union", UNION; "inter", INTER; "diff", DIFF;
        "let", LET; "in", IN; "readrle", READRLE;
        "readmc", READMC; "outrle", OUTRLE; "outmc", OUTMC;
        "iter", ITER; "mirror", MIRROR; "rotate90", ROT90;
        "rotate180", ROT180; "rotate270", ROT270;
        "extend", EXTEND; "extend_left", LXTD; "extend_right", RXTD
      ];
    fun s -> try Hashtbl.find h (String.lowercase s) with Not_found -> IDENT s

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let char = (alpha | digit | '_' | '-' | '/' | '.')
let ident = (alpha | '_') (alpha | '_' | digit)*
let space = (' ' | '\t' | '\r' )

rule token = parse
  | space+
      { token lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; token lexbuf }
  | ident as s
      { id_or_keyword s }
  | digit+ as i
      { NUMBER (int_of_string i) }
  | '\"' 
      { Buffer.reset str_buff; string lexbuf }
  | "="
      { EQUALS }
  | "("
      { LP }
  | ")" 
      { RP }
  | ","
      { COMMA }
  | "(*" 
      { comment lexbuf }
  | eof
      { EOF }
  | _ as c 
      { raise (Lexical_error ("illegal character " ^ Char.escaped c )) }

and string = parse 
  | char as c
      { Buffer.add_char str_buff c; string lexbuf }
  | '\"'
      { FILENAME (Buffer.contents str_buff) }
  | eof
      { raise (Lexical_error "unterminated string") }
  | _ as c
      { raise (Lexical_error ("illegal character " ^ Char.escaped c )) }


and comment = parse
  | "*)" 
      { token lexbuf }
  | _ 
      { comment lexbuf }
