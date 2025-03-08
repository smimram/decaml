{
open Lexing
open Parser
(* open LexingUtils *)
}

let space = ' ' | '\t' | '\r'

let first_char = ['_''A'-'Z''a'-'z']
let char = first_char | ['0'-'9']

rule token = parse
  | "let" { LET }
  | "in" { IN }
  | ":" { COLON }
  | "=" { EQ }
  | "->" { TO }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LACC }
  | "}" { RACC }
  | "_" { HOLE }
  | "type" { TYPE }
  | (['0'-'9']+ as n) { INT (int_of_string n) }
  | (first_char char* as s) { IDENT s }
  | "(*"[^'*']*"*)" { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
