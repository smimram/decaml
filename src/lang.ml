open Common

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

module Context = struct
  type t =
    {
      environment : V.environment;
      level : int;
      types : (string * V.ty) list;
    }

  let empty =
    {
      environment = [];
      level = 0;
      types = [];
    }

  let bind ctx x a =
    {
      environment = (V.var ctx.level)::ctx.environment;
      level = ctx.level + 1;
      types = (x,a) :: ctx.types;
    }

  (* close : (Γ : Con) → Val (Γ, x : A) B → Closure Γ A B *)
  let close ctx (t:value) : V.closure = ctx.environment, V.quote (ctx.level + 1) t
end

let rec infer (ctx:Context.t) (t:preterm) : term * ty =
  match t.desc with
  | Abs ((x,i,a),t) ->
    let a = check ctx a Type in
    let a = V.eval ctx.Context.environment a in
    let ctx' = Context.bind ctx x a in
    let t, b = infer ctx' t in
    T.Abs ((x,i),t), V.Pi((x,i,a),Context.close ctx b)
  | App (t,(i,u)) ->
    let t, c = infer ctx t in
    let a,(env,b) =
      match c with
      | Pi ((_,i',a),(env,b)) ->
        if i <> i' then failwith "TODO: support implicit parameters";
        a,(env,b)
      | _ -> failwith "function expected"
    in
    let u = check ctx u a in
    App (t,(i,u)), V.eval ((V.eval ctx.environment u)::env) b
  | Var x ->
    let n, a =
      let rec aux n = function
        | (y,a)::l -> if x = y then n, a else aux (n+1) l
        | [] -> failwith "error"
      in
      aux 0 ctx.Context.types
    in
    Var n, a
  | Pi ((x,i,a),b) ->
    let a = check ctx a Type in
    let b = check (Context.bind ctx x (V.eval ctx.environment a)) b Type in
    Pi ((x,i,a),b), Type
  | Type ->
    Type, Type

and check (ctx:Context.t) (t:preterm) (a:ty) : term =
  let t, a' = infer ctx t in
  V.unify ctx.Context.level a' a;
  t
