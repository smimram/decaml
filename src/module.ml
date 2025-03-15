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
  (* let ind name parameters ty constructors d = (Ind {P.name; parameters; ty; constructors})::d in *)
  (* ind "unit" [] (mk Type) ["U", mk (Var "unit")] @@ *)
  (* ind "bool" [] (mk Type) ["true", mk (Var "bool"); "false", mk (Var "bool")] @@ *)
  def "nat" Nat @@
  def "Z" Z @@
  def "S" S @@
  d

let eval_decl ctx d =
  let open Lang in
  match d with
  | Def (r,x,a,t) ->
    let t = if r then mk (Fix (mk (Abs ((x,`Explicit,a),t)))) else t in
    (* Printf.printf "%s = %s\n%!" x (Preterm.to_string t); *)
    let t, a =
      match a with
      | Some a ->
        let a = check ctx a Type in
        let a = eval ctx a in
        check ctx t a, a
      | None ->
        infer ctx t
    in
    Printf.printf "%s : %s\n%!" x (to_string ctx a);
    Printf.printf "%s = %s\n%!" x (T.to_string (Context.variables ctx) t);
    Printf.printf "%s = %s\n%!" x (T.to_string (Context.variables ctx) (normalize ctx t));
    print_newline ();
    (* Context.bind ctx x a *)
    Context.define ctx x (V.eval ctx.environment t) a
  | Ind ind ->
    Printf.printf "inductive %s : %s\n\n%!" ind.Preterm.name (String.concat " | " @@ List.map fst ind.constructors);
    (* TODO: add arguments parameters to the type *)
    let ty = eval ctx @@ check ctx ind.ty Type in
    let rec inductive () : V.inductive =
      let id = V.fresh_ind () in
      let me = V.Ind (ind.Preterm.name, id, inductive) in
      let ctx = Context.define ctx ind.name me ty in
      { Value.
        id = id;
        name = ind.name;
        ty = ty;
        constructors =
          List.map
            (fun (c,a) ->
               (* Printf.printf "check %s\n%!" (P.to_string a); *)
               let a = check ctx a Type in
               (* Printf.printf "checked\n%!"; *)
               c, eval ctx a
            ) ind.constructors;
      }
    in
    Context.inductive ctx (inductive ())

let eval ctx (m : t) = List.fold_left eval_decl ctx m
