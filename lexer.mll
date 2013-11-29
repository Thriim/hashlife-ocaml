
{
  open Parser
}

let digit = ['0'-'9']
let int = (digit+)
let space = (' '|'\t'|'\n')*
let comment = '#' [^'\n']* '\n'

rule token = parse
  | comment                                 { token lexbuf }
  | 'x' space '=' space (int as x) space ','{ X(int_of_string x) }
  | 'y' space '=' space (int as y) space ','{ Y(int_of_string y) }
  | "rule" space '=' space 'B' int "/S" int { RULE }
  | (int as n) 'o'                          { ON(int_of_string n) }
  | (int as n) 'b'                          { OFF(int_of_string n) }
  | (int as n) '$'                          { NEWLINE(int_of_string n) }
  | 'o'                                     { ON(1) }
  | 'b'                                     { OFF(1) }
  | '$'                                     { NEWLINE(1) }
  | '!'                                     { token lexbuf }
  | space                                   { token lexbuf }
  | eof                                     { EOF }
  | _                                       { failwith ("Lexer Failure") }
