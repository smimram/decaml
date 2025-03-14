open Extlib

module P = Preterm
module T = Term
module V = Value

open Term

type preterm = P.t
type term = T.t
type value = V.t
type ty = V.t

exception Type_error of Pos.t * string

let type_error pos = Printf.ksprintf (fun s -> raise (Type_error (pos, s)))

(** Typing and evaluation contexts. *)
module Context = struct

  (** The contexts for type inference. *)
  type t =
    {
      environment : V.environment; (** the evaluation environment *)
      level : int; (** level for creating fresh variables for abstractions *)
      types : (string * V.ty) list; (** the typing environment *)
      bds : [`Bound | `Defined] list; (** whether variables of the environment are defined or bound (this is used for metavariables which only depend on bound variables) *)
    }

  (** Empty context. *)
  let empty =
    {
      environment = [];
      level = 0;
      types = [];
      bds = [];
    }

  (** Variables defined in the context. *)
  let variables ctx =
    List.map fst ctx.types

  (** Declare a bound variable of given type. *)
  let bind ctx x a =
    {
      environment = (V.var ctx.level)::ctx.environment;
      level = ctx.level + 1;
      types = (x,a)::ctx.types;
      bds = `Bound::ctx.bds;
    }

  (** Insert a new binding. *)
  let new_binder = bind

  (** Define a term to a given value and type. *)
  let define ctx x t a =
    {
      environment = t::ctx.environment;
      level = ctx.level + 1;
      types = (x,a)::ctx.types;
      bds = `Defined::ctx.bds;
    }

  (* close : (Γ : Con) → Val (Γ, x : A) B → Closure Γ A B *)
  let close ctx (t:value) : V.closure = ctx.environment, V.quote (ctx.level + 1) t
end

let fresh_meta (ctx:Context.t) =
  let m = Value.fresh_meta () in
  InsertedMeta (m.id, ctx.bds)

let eval ctx (t : term) : value = V.eval ctx.Context.environment t

let quote ctx (t : value) : term = V.quote ctx.Context.level t

let normalize ctx t = quote ctx @@ eval ctx t

let to_string ctx t = T.to_string (Context.variables ctx) @@ quote ctx t

(** Apply all implicit arguments to metavariables. *)
let rec insert ctx (t:term) (a:ty) =
  match V.force a with
  | Pi((_,`Implicit,_),(env,b)) ->
    let m = fresh_meta ctx in
    let m' = eval ctx m in
    let t = App (t,(`Implicit,m)) in
    let a = V.eval (m'::env) b in
    insert ctx t a
  | _ -> t, a

let rec infer (ctx:Context.t) (t:preterm) : term * ty =
  (* Printf.printf "*** infer %s\n%!" (P.to_string t); *)
  let pos = t.pos in
  match t.desc with
  | Let (x,a,t,u) ->
    let t, a =
      match a with
      | Some a ->
        let a = eval ctx @@ check ctx a V.Type in
        let t = check ctx t a in
        t, a
      | None ->
        infer ctx t
    in
    let u, b = infer (Context.define ctx x (eval ctx t) a) u in
    (* TODO: check this quote *)
    let a = quote ctx a in
    Let (x,a,t,u), b
  | Abs ((x,i,a),t) ->
    let a =
      match a with
      | Some a -> check ctx a V.Type
      | None -> fresh_meta ctx
    in
    let a = eval ctx a in
    let ctx' = Context.bind ctx x a in
    let t, b = infer ctx' t in
    T.Abs ((x,i),t), V.Pi((x,i,a), Context.close ctx b)
  | App (t,(i,u)) ->
    let tpos = t.pos in
    let t, c =
      match i with
      | `Implicit -> infer ctx t
      | `Explicit ->
        (* Apply all implicit arguments to metavariables. *)
        let rec aux ((t:term),(c:ty)) =
          match V.force c with
          | Pi((_,`Implicit,_),(env,b)) ->
            let m = fresh_meta ctx in
            let m' = eval ctx m in
            let t = App (t,(`Implicit,m)) in
            let c = V.eval (m'::env) b in
            aux (t, c)
          | _ -> t,c
        in
        aux @@ infer ctx t
    in
    let a,(env,b) =
      match V.force c with
      | Pi ((_,i',a),(env,b)) ->
        if i <> i' then failwith "TODO: support implicit parameters";
        a,(env,b)
      | _ ->
        (* We use unification here because c might be a metavariable. *)
        let a = fresh_meta ctx in
        let b = fresh_meta (Context.bind ctx "x#" (eval ctx a)) in
        let c' = eval ctx (Pi(("x#",i,a),b)) in
        unify tpos ctx c c';
        (
          match V.force c' with
          | Pi ((_,_,a),(env,b)) -> a,(env,b)
          | _ -> assert false
        )
    in
    let u = check ctx u a in
    App (t,(i,u)), V.eval ((eval ctx u)::env) b
  | Var x ->
    let n, a =
      let rec aux n = function
        | (y,a)::l -> if x = y then (n,a) else aux (n+1) l
        | [] -> type_error pos "unbound variable: %s" x
      in
      aux 0 ctx.types
    in
    Var n, a
  | Pi ((x,i,a),b) ->
    let a = match a with Some a -> check ctx a Type | None -> fresh_meta ctx in
    let b = check (Context.bind ctx x (eval ctx a)) b Type in
    Pi ((x,i,a),b), Type
  | Fix t ->
    let t, c = infer ctx t in
    (
      match c with
      | Pi ((_,`Explicit,a),(env,b)) ->
        let b =
          let f = V.Fix ((env,t), []) in
          V.eval (f::env) b
        in
        unify pos ctx b a;
        Fix t, a
      | _ -> assert false
    )
  | Hole ->
    let t = fresh_meta ctx in
    let a = eval ctx @@ fresh_meta ctx in
    t, a
  | Cast (t, a) ->
    let a = check ctx a Type in
    let a = eval ctx a in
    let t = check ctx t a in
    t, a
  | Type -> Type, Type
  | Unit -> Unit, Type
  | U -> U, Unit
  | Nat -> Nat, Type
  | Z -> Z, Nat
  | S -> S, V.arr Nat Nat

and check (ctx:Context.t) (t:preterm) (a:ty) : term =
  (* Printf.printf "*** check %s : %s\n%!" (P.to_string t) (to_string ctx a); *)
  let pos = t.pos in
  match t.desc, V.force a with

  | Abs ((x,i,a),t), Pi ((_x',i',a'),(env,b)) when i = i' ->
    if a <> None then
      (
        let a = Option.get a in
        let pos = a.pos in
        let a = check ctx a Type in
        let a = eval ctx a in
        unify pos ctx a a'
      );
    let b = V.eval ((V.var ctx.level)::env) b in
    let t = check (Context.bind ctx x a') t b in
    Abs ((x,i),t)

  | _, Pi ((x,`Implicit,a),(env,b)) when not (P.is_fix t) ->

    (* Insert an implicit abstraction. *)
    let b = V.eval ((V.var ctx.level)::env) b in
    let t = check (Context.new_binder ctx x a) t b in
    Abs ((x,`Implicit),t)

  | Let (x,a,t,u), a' ->
    let a =
      match a with
      | Some a -> check ctx a Type
      | None -> fresh_meta ctx
    in
    let va = eval ctx a in
    let t = check ctx t va in
    let vt = eval ctx t in
    let u = check (Context.define ctx x vt va) u a' in
    Let (x,a,t,u)

  | Fix t, b ->
    let t = check ctx t (V.arr b (quote ctx b)) in
    Fix t

  | _, a' ->
    let t, a = infer ctx t in
    let t, a = insert ctx t a in
    unify pos ctx a a';
    t

and unify pos (ctx:Context.t) (a:ty) (b:ty) =
  (* Printf.printf "*** unify %s with %s\n%!" (to_string ctx a) (to_string ctx b); *)
  if not @@ V.unify ctx.Context.level a b then
    type_error pos "expression has type %s but type %s was expected" (to_string ctx a) (to_string ctx b)
