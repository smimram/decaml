open Lexing

let advance_pos n lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }

(** Number of utf8 characters. *)
let utf8_length s =
  if not (String.is_valid_utf_8 s) then String.length s else
    let rec aux n i =
      if i >= String.length s then n else
        let c = String.get_utf_8_uchar s i in
        aux (n + 1) (i + Uchar.utf_decode_length c)
    in
    aux 0 0

(** Correct position for strings with utf8 characters. *)
let utf8 s lexbuf = advance_pos (String.length s - utf8_length s) lexbuf
