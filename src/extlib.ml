module List = struct
  include List

  let rec fold_left_map f s l =
    match l with
    | x::l ->
      let s, x = f s x in
      let s, l = fold_left_map f s l in
      s, x::l
    | [] -> s, l
end

(** Positions in files. *)
module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy : t = Lexing.dummy_pos, Lexing.dummy_pos
end
