(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term

open P

(** Toplevel declarations. *)
type decl =
  | Def of (var * P.t)

(** A module. *)
type t = decl list

(** Add standard prelude. *)
let prelude (d:t) : t =
  let def x t d = (Def (x, mk t))::d in
  def "unit" Unit @@
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d
