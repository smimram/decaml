(** Modules are sequences of declarations. *)

module P = Preterm
module T = Term
module V = Value

open P

(** Toplevel declarations. *)
type decl =
  | Def of (bool * var * P.ty option * P.t) (** a declaration, which might be recursive *)
  | Ind of P.inductive

(** A module. *)
type t = decl list

(** Add standard prelude. *)
let prelude (d:t) : t =
let def x t d = (Def (false, x, None, mk t))::d in
  let ind n c d =
    (Ind {P.name = n; constructors = c})::d
  in
  ind "unit" ["U", [], mk (Var "unit")] @@
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d

let eval_decl ctx = function
  | Def (r,x,a,t) ->
    let t = if r then mk (Fix (mk (Abs ((x,`Explicit,a),t)))) else t in
    (* Printf.printf "%s = %s\n%!" x (Preterm.to_string t); *)
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
    Printf.printf "%s = %s\n%!" x (T.to_string (Lang.Context.variables ctx) (Lang.normalize ctx t));
    print_newline ();
    (* Lang.Context.bind ctx x a *)
    Lang.Context.define ctx x (V.eval ctx.environment t) a
  | Module.Ind ind ->
    Printf.printf "inductive %s" ind.Preterm.name;
    let ind : Value.inductive =
      { Value.
        name = ind.name;
        ty = Value.eval ctx.environment ty;
        constructors = List.map (fun (c,a) -> c, Value.eval ctx.environment a) ind.constructors;
      }
    in
    Lang.Context.inductive ctx ind

let eval ctx (m : t) = List.fold_left eval_decl ctx m
