(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term

open P

type decl =
  | Def of (var * P.t)

type t = decl list

let prelude d =
  let def x t d = (Def (x, mk t))::d in
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d
