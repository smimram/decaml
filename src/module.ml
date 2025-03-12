(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term

open P

(** Toplevel declarations. *)
type decl =
  | Def of (var * P.t)
  | Ind of P.inductive

(** A module. *)
type t = decl list

(** Add standard prelude. *)
let prelude (d:t) : t =
  let def x t d = (Def (x, mk t))::d in
  let ind n c d =
    (Ind {P.name = n; constructors = c})::d
  in
  ind "unit" ["U", [], mk (Var "unit")] @@
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d
