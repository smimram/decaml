{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'

let first_char = ['_''A'-'Z''a'-'z''0'-'9']
let char = first_char | ['0'-'9']

rule token = parse
  | "let" { LET }
  | "in" { IN }
  | ":" { COLON }
  | "=" { EQ }
  | "(" { LPAR }
  | ")" { RPAR }
  | "_" { HOLE }
  | "|" { VBAR }
  | "type" { TYPE }
  | (first_char char* as s) { IDENT s }
  | "(*"[^'*']*"*)" { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
