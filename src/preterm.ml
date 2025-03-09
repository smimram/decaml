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
  | Let of var * ty option * t * t
  | Abs of (var * icit * ty) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of var
  | Pi of (var * icit * ty) * t
  | Type (** the type of types *)
  | Hole
  | Cast of t * ty (** ensure that a term has given type *)

  | Unit | U
  | Nat | Z | S

and ty = t

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

let rec nat ?pos n =
  if n < 0 then failwith "natural numbers must be positive"
  else if n = 0 then mk ?pos Z
  else mk ?pos (App (mk ?pos S, (`Explicit, nat ?pos (n-1))))

(*
(** Let declaration. *)
let letin ?pos x t a u =
  let pos = Option.value ~default:a.pos pos in
  mk ~pos (App (mk ~pos (Abs ((x, `Explicit, a), u)), (`Explicit, t)))
*)
 
let rec to_string ?(pa=false) e =
  let pa s = if pa then "("^s^")" else s in
  match e.desc with
  | Let (x,a,t,u) ->
    let a = match a with Some a -> " : " ^ to_string a | None -> "" in
    Printf.sprintf "let %s%s = %s in\n%s" x a (to_string t) (to_string u)
  | Abs ((x,i,t), e) ->
    let arg = icit_pa i (x ^ " : " ^ to_string t) in
    pa (Printf.sprintf "fun %s -> %s" arg (to_string e))
  | App (f,(i,e)) ->
    let e =
      match i with
      | `Explicit -> to_string ~pa:true e
      | `Implicit -> "{" ^ to_string e ^ "}"
    in
    pa (Printf.sprintf "%s %s" (to_string f) e)
  | Pi ((x,i,t),e) ->
    let arg = icit_pa i (x ^ " : " ^ to_string t) in
    pa (Printf.sprintf "Π %s -> %s" arg (to_string e))
  | Var x -> x
  | Hole -> "_"
  | Cast (t,a) -> Printf.sprintf "(%s : %s)" (to_string t) (to_string a)
  | Type -> "type"
  | Unit -> "unit"
  | U -> "()"
  | Nat -> "nat"
  | Z -> "Z"
  | S -> "S"

(*
let string_of_decl = function
  | Def (x, v) -> Printf.sprintf "let %s = %s" x (to_string v)
*)
