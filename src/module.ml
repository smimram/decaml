(** Modules are sequences of declarations. *)

module T = Preterm

open T

type t =
  | Def of (var * T.t)

let prelude d =
  let def x t d = (Def (x, mk t))::d in
  def "Nat" Nat @@
  def "zero" Z @@
  def "suc" S @@
  d
