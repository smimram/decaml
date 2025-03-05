(** Positions in files. *)
module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy : t = Lexing.dummy_pos, Lexing.dummy_pos
end

type icit = [`Explicit | `Implicit]

let todo () = failwith "TODO"
