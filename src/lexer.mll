{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "let" { LET }
  | "in" { IN }
  | ":" { COLON }
  | "=" { EQ }
  | "(" { LPAR }
  | ")" { RPAR }
  | "_" { HOLE }
  | "|" { VBAR }
  | "Type" { TYPE }
  | (['A'-'Z''a'-'z''0'-'9']+ as s) { IDENT s }
  | "(*"[^'*']*"*)" { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
