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
  | Let of def * t
  | Abs of pattern * t * t (** λ-abstraction *)
  | App of t * t
  | Pi of pattern * t * t
  | Var of var
  | Cast of t * t (** cast an expression to a given type *)
  | Type (** the type of types *)
  | Cons of cons * t (** constructor of given type *)

(** A pattern. *)
and pattern =
  | PVar of var

and decl =
  | Def of def
  | Ind of ind (** an inductive type *)

and def = pattern * t

(** An inductive type. *)
and ind =
  {
    ind_name : cons; (** name of the inductive type *)
    ind_param : (pattern * t) list; (** parameters *)
    ind_type : t; (** type of the type constructor (without parameters) *)
    ind_cons : (cons * t) list; (** constructors *)
  }

let mk ?pos desc =
  let pos = Option.value ~default:Pos.dummy pos in
  { pos; desc }

let abs ?pos x t e =
  mk ?pos (Abs (x, t, e))

let pi ?pos x t e =
  mk ?pos (Pi (x, t, e))

let app ?pos f e =
  mk ?pos (App (f, e))

let var ?pos x =
  mk ?pos (Var x)

let typ ?pos () =
  mk ?pos Type

let cast ?pos e t =
  mk ?pos (Cast (e, t))

let letin ?pos (p, v) e =
  mk ?pos (Let ((p, v), e))

let cons ?pos c a =
  mk ?pos (Cons (c, a))

(** Multiple abstractions. *)
let abss ?pos a e =
  let pos = Option.value ~default:e.pos pos in
  let rec aux = function
    | [] -> e
    | (x,t)::l -> abs ~pos x t (aux l)
  in
  aux a

(** Multiple pi types. *)
let pis ?pos args a =
  let pos = Option.value ~default:a.pos pos in
  let rec aux = function
    | [] -> a
    | (x,a)::l -> pi ~pos x a (aux l)
  in
  aux args

let string_of_pattern = function
  | PVar x -> x

let rec to_string ?(pa=false) e =
  let pa s = if pa then "("^s^")" else s in
  match e.desc with
  | Let ((p, v), e) -> pa (Printf.sprintf "let %s = %s in %s" (string_of_pattern p) (to_string v) (to_string e))
  | Abs (p, t, e) -> pa (Printf.sprintf "fun (%s : %s) -> %s" (string_of_pattern p) (to_string t) (to_string e))
  | App (f, e) -> pa (Printf.sprintf "%s %s" (to_string f) (to_string ~pa:true e))
  | Pi (p, t, e) -> pa (Printf.sprintf "(%s : %s) -> %s" (string_of_pattern p) (to_string t) (to_string e))
  | Var x -> x
  | Type -> "Type"
  | Cast (e, t) -> Printf.sprintf "(%s : %s)" (to_string e) (to_string t)
  | Cons (c, _) -> c

let string_of_decl = function
  | Def (p, v) -> Printf.sprintf "let %s = %s" (string_of_pattern p) (to_string v)
  | Ind i ->
    let name = i.ind_name in
    let param = List.map (fun (p, a) -> Printf.sprintf "(%s : %s)" (string_of_pattern p) (to_string a)) i.ind_param in
    let param = String.concat " " param in
    let cons = List.map (fun (c, a) -> Printf.sprintf "| %s : %s" c (to_string a)) i.ind_cons in
    let cons = String.concat " " cons in
    Printf.sprintf "type %s %s : %s = %s" name param (to_string i.ind_type) cons
