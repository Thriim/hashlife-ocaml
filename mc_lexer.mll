
{
  open Mc_parser
}

let digit = ['0'-'9']
let int = (digit+)
let space = (' '|'\t'|'\r')*
let comment = '#' [^'\n']* '\n'

rule token = parse
  | '[' [^'\n']* '\n'            { token lexbuf }
  | comment                      { token lexbuf }
  | int as i                     { INT(int_of_string i) }
  | '*'                          { ON }
  | '.'                          { OFF }
  | '$'                          { NEWLINE }
  | '\n'                         { ENDLINE }
  | space                        { token lexbuf }
  | eof                          { EOF }
  | _ as str                     { failwith ("Lexer Failure " ^ (Char.escaped str)) }
