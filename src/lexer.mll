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
  | "fun" { FUN }
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
  | "(*" { comment 0 lexbuf; token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }

and comment depth = parse
  | "(*" { comment (depth+1) lexbuf }
  | "*)" { if depth > 0 then comment (depth-1) lexbuf }
  | _    { comment depth lexbuf }
