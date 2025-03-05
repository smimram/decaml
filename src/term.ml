(** Terms are expressions we get out of the parser. *)

open Extlib

(** A variable. *)
type var = string

(** A constructor. *)
type cons = string

(** An expression. *)
type t =
  {
    pos : Pos.t;
    desc : desc;
  }

(** The contents of an expression. *)
and desc =
  | Let of var * t * t
  | Abs of var * t * t (** λ-abstraction *)
  | App of t * t
  | Pi of var * t * t
  | Var of var
  | Cast of t * t (** cast an expression to a given type *)
  | Type (** the type of types *)
  | Cons of cons * t (** constructor of given type *)

and decl =
  | Def of var * t

let mk ?pos desc =
  let pos = Option.value ~default:Pos.dummy pos in
  { pos; desc }

(** Multiple abstractions. *)
let abss ?pos a e =
  let pos = Option.value ~default:e.pos pos in
  let rec aux = function
    | [] -> e
    | (x,t)::l -> mk ~pos (Abs (x, t, aux l))
  in
  aux a

(** Multiple pi types. *)
let pis ?pos args a =
  let pos = Option.value ~default:a.pos pos in
  let rec aux = function
    | [] -> a
    | (x,a)::l -> mk ~pos (Pi (x, a, aux l))
  in
  aux args

let rec to_string ?(pa=false) e =
  let pa s = if pa then "("^s^")" else s in
  match e.desc with
  | Let (x, v, e) -> pa (Printf.sprintf "let %s = %s in %s" x (to_string v) (to_string e))
  | Abs (x, t, e) -> pa (Printf.sprintf "fun (%s : %s) -> %s" x (to_string t) (to_string e))
  | App (f, e) -> pa (Printf.sprintf "%s %s" (to_string f) (to_string ~pa:true e))
  | Pi (x, t, e) -> pa (Printf.sprintf "(%s : %s) -> %s" x (to_string t) (to_string e))
  | Var x -> x
  | Type -> "Type"
  | Cast (e, t) -> Printf.sprintf "(%s : %s)" (to_string e) (to_string t)
  | Cons (c, _) -> c

let string_of_decl = function
  | Def (x, v) -> Printf.sprintf "let %s = %s" x (to_string v)
