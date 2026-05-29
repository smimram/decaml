open Parser

exception Error of string

let digit = [%sedlex.regexp? '0'..'9']
let first_char = [%sedlex.regexp? '_' | 'A'..'Z' | 'a'..'z']
let ident_char = [%sedlex.regexp? first_char | digit]

let rec token buf =
  match%sedlex buf with
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "fun" -> FUN
  | "match" -> MATCH
  | "with" -> WITH
  | "inductive" -> INDUCTIVE
  | "begin" -> BEGIN
  | "end" -> END
  | "|" -> BAR
  | ":" -> COLON
  | "=" -> EQ
  | "->" -> TO
  | "(" -> LPAR
  | ")" -> RPAR
  | "{" -> LACC
  | "}" -> RACC
  | "_" -> HOLE
  | "type" -> TYPE
  | Plus digit -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | first_char, Star ident_char -> IDENT (Sedlexing.Utf8.lexeme buf)
  | "(*" -> comment 0 buf; token buf
  | Plus (' ' | '\t' | '\r') -> token buf
  | '\n' -> token buf
  | eof -> EOF
  | _ -> raise (Error (Printf.sprintf "Unexpected character: %s" (Sedlexing.Utf8.lexeme buf)))

and comment depth buf =
  match%sedlex buf with
  | "(*" -> comment (depth + 1) buf
  | "*)" -> if depth > 0 then comment (depth - 1) buf
  | '\n' -> comment depth buf
  | eof -> raise (Error "Unterminated comment")
  | any -> comment depth buf
  | _ -> assert false
