(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term
module V = Value

open P

(** Toplevel declarations. *)
type decl =
  | Def of (bool * var * P.ty option * P.t) (** a declaration, which might be recursive *)

(** A module. *)
type t = decl list

(** Add standard prelude. *)
let prelude (d:t) : t =
  let def x t d = (Def (false, x, None, mk t))::d in
  def "unit" Unit @@
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d

let eval_decl ctx = function
  | Def (r,x,a,t) ->
    let t = if r then mk (Fix (mk (Abs ((x,`Explicit,a),t)))) else t in
    Printf.printf "%s = %s\n%!" x (Preterm.to_string t);
    let t, a =
      match a with
      | Some a ->
        let a = Lang.check ctx a Type in
        let a = Lang.eval ctx a in
        Lang.check ctx t a, a
      | None ->
        Lang.infer ctx t
    in
    Printf.printf "%s : %s\n%!" x (Lang.to_string ctx a);
    Printf.printf "%s = %s\n%!" x (T.to_string (Lang.Context.variables ctx) t);
    print_newline ();
    (* Lang.Context.bind ctx x a *)
    Lang.Context.define ctx x (V.eval ctx.environment t) a

let eval ctx (m : t) = List.fold_left eval_decl ctx m
