let failwith fmt = Printf.ksprintf failwith fmt

(* Very naive implementation, for backward compatibility... *)
module Dynarray = struct
  type 'a t = 'a list ref

  let create () : 'a t = ref []

  let length (a : 'a t) = List.length !a

  let add_last (a : 'a t) x = a := x :: !a

  let get (a : 'a t) n = List.nth !a (length a - 1 - n)
end

module String = struct
  include String

  (** Number of utf8 characters. *)
  let utf8_length s =
    if not (String.is_valid_utf_8 s) then String.length s else
      let rec aux n i =
        if i >= String.length s then n else
          let c = String.get_utf_8_uchar s i in
          aux (n + 1) (i + Uchar.utf_decode_length c)
      in
      aux 0 0
end

module Lexing = struct
  include Lexing

  let advance_pos n lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }

  (** Correct position for strings with utf8 characters. *)
  let utf8 s lexbuf =
    advance_pos (String.length s - String.utf8_length s) lexbuf
end

module List = struct
  include List

  let index p l =
    let rec aux n = function
      | x::l -> if p x then n else aux (n+1) l
      | [] -> raise Not_found
    in
    aux 0 l

  let rec filter_map2 f l1 l2 =
    match l1, l2 with
    | x1::l1, x2::l2 ->
      (
        match f x1 x2 with
        | Some y -> y::(filter_map2 f l1 l2)
        | None -> filter_map2 f l1 l2
      )
    | [], [] -> []
    | _ -> assert false
end

(** Positions in the source code. *)
module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy = Lexing.dummy_pos, Lexing.dummy_pos

  let union (p1,p2) (q1,q2) =
    assert (p1.Lexing.pos_fname = q1.Lexing.pos_fname);
    let r1 = if p1.Lexing.pos_cnum <= q1.Lexing.pos_cnum then p1 else q1 in
    let r2 = if p2.Lexing.pos_cnum >= q2.Lexing.pos_cnum then p2 else q2 in
    r1,r2

  (** String representation of a position. *)
  let to_string ((p1,p2):t) =
    let l1 = p1.Lexing.pos_lnum in
    let l2 = p2.Lexing.pos_lnum in
    let b1 = p1.Lexing.pos_bol in
    let b2 = p2.Lexing.pos_bol in
    let c1 = p1.Lexing.pos_cnum in
    let c2 = p2.Lexing.pos_cnum in
    let c1 = c1 - b1 in
    let c2 = c2 - b2 in
    (
      if p1.Lexing.pos_fname <> "" then
        Printf.sprintf "in file %s " p1.Lexing.pos_fname
      else
        ""
    ) ^
      if l1 = l2 then
        if c1 = c2 then
          Printf.sprintf "line %d character %d" l1 c1
        else
          Printf.sprintf "line %d characters %d-%d" l1 c1 c2
      else
        Printf.sprintf "from line %d character %d to line %d character %d" l1 c1 l2 c2

  module Option = struct
    type nonrec t = t option

    let to_string = function
      | Some pos -> to_string pos
      | None -> "unknown position"
  end
end
