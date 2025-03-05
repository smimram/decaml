open Common
open Extlib

module P = Preterm
module T = Term
module V = Value

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
end

let infer (ctx:Context.t) (t:preterm) : term * ty =
  match t.desc with
  | Abs ((x,i,a),t) -> _
    (* let  *)
    (* T.Abs ((x,i),t), V.Pi _ *)
  | App (_, _) -> _
  | Var x ->
    let n, a =
      let rec aux n = function
        | (y,a)::l -> if x = y then n, a else aux (n+1) l
        | [] -> failwith "error"
      in
      aux 0 ctx.Context.types
    in
    T.Var n, a
  | Pi (_, _) -> _
  | Type -> _
