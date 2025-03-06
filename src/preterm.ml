(** Pre-terms are what we get out of the parser. *)

open Common
open Extlib

(** A variable. *)
type var = string

(** An expression. *)
type t =
  {
    pos : Pos.t;
    desc : desc;
  }

(** The contents of an expression. *)
and desc =
  | Abs of (var * icit * ty) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of var
  | Pi of (var * icit * ty) * t
  | Type (** the type of types *)

  | Z | S

and ty = t

and decl =
  | Def of (var * t)

let mk ?pos desc =
  let pos = Option.value ~default:Pos.dummy pos in
  { pos; desc }

(** Multiple abstractions. *)
let abss ?pos a e =
  let pos = Option.value ~default:e.pos pos in
  let rec aux = function
    | [] -> e
    | x::l -> mk ~pos (Abs (x, aux l))
  in
  aux a

(** Multiple pi types. *)
let pis ?pos args a =
  let pos = Option.value ~default:a.pos pos in
  let rec aux = function
    | [] -> a
    | x::l -> mk ~pos (Pi (x, aux l))
  in
  aux args

(*
(** Let declaration. *)
let letin ?pos x t a u =
  let pos = Option.value ~default:a.pos pos in
  mk ~pos (App (mk ~pos (Abs ((x, `Explicit, a), u)), (`Explicit, t)))
*)
 
(*
let rec to_string ?(pa=false) e =
  let icit i s =
    match i with
    | `Explicit -> "("^s^")"
    | `Implicit -> "{"^s^"}"
  in
  let pa s = if pa then "("^s^")" else s in
  match e.desc with
  | Abs ((x,i,t), e) ->
    let arg = icit i (x ^ " : " ^ to_string t) in
    pa (Printf.sprintf "fun %s -> %s" arg (to_string e))
  | App (f,(i,e)) ->
    let e =
      match i with
      | `Explicit -> to_string ~pa:true e
      | `Implicit -> "{" ^ to_string e ^ "}"
    in
    pa (Printf.sprintf "%s %s" (to_string f) e)
  | Pi ((x,i,t),e) ->
    let arg = icit i (x ^ " : " ^ to_string t) in
    pa (Printf.sprintf "%s -> %s" arg (to_string e))
  | Var x -> x
  | Type -> "Type"

let string_of_decl = function
  | Def (x, v) -> Printf.sprintf "let %s = %s" x (to_string v)
*)

let prelude d =
  let def x t d = (Def (x, mk t))::d in
  def "zero" Z @@
  def "suc" S @@
  d
