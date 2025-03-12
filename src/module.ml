(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term

open P

(** Toplevel declarations. *)
type decl =
  | Def of (bool * var * P.t) (** a declaration, which might be recursive *)

(** A module. *)
type t = decl list

(** Add standard prelude. *)
let prelude (d:t) : t =
  let def x t d = (Def (false, x, mk t))::d in
  def "unit" Unit @@
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d
