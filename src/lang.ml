module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy : t = Lexing.dummy_pos, Lexing.dummy_pos
end

type var = string

module Expr = struct
  type t =
    {
      pos : Pos.t;
      desc : desc;
    }

  and desc =
    | Let of pattern * t * t
    | Abs of pattern * t * t
    | App of t * t
    | Pi of pattern * t * t
    | Var of var
    | Cast of t * t (** cast an expression to a given type *)
    | Type

  and pattern =
    | PVar of var

  let mk ?pos desc =
    let pos = Option.value ~default:Pos.dummy pos in
    { pos; desc }

  let abs ?pos x t e =
    mk ?pos (Abs (x, t, e))

  let var ?pos x =
    mk ?pos (Var x)

  let typ ?pos () =
    mk ?pos Type

  let cast ?pos e t =
    mk ?pos (Cast (e, t))

  (** Multiple abstractions. *)
  let abss ?pos a e =
    let pos = Option.value ~default:e.pos pos in
    let rec aux = function
      | [] -> e
      | (x,t)::l -> abs ~pos x t (aux l)
    in
    aux a

  let string_of_pattern = function
    | PVar x -> x

  let rec to_string e =
    match e.desc with
    | Let (p, v, e) -> Printf.sprintf "let %s = %s in %s" (string_of_pattern p) (to_string v) (to_string e)
    | Abs (p, t, e) -> Printf.sprintf "(fun (%s : %s) -> %s)" (string_of_pattern p) (to_string t) (to_string e)
    | App (f, e) -> Printf.sprintf "(%s %s)" (to_string f) (to_string e)
    | Pi (p, t, e) -> Printf.sprintf "((%s : %s) -> %s)" (string_of_pattern p) (to_string t) (to_string e)
    | Var x -> x
    | Type -> "Type"
    | Cast (e, t) -> Printf.sprintf "(%s : %s)" (to_string e) (to_string t)
end

include Expr

let string_of_expr = Expr.to_string

module Value = struct
  type nonrec var = var

  type t =
    | Abs of t * (t -> t)
    | Pi of t * (t -> t)
    | Type
    | Neutral of neutral
  and neutral =
    | Var of var
    | App of neutral * t
    | Cast of neutral * t

  let is_neutral = function
    | Neutral _ -> true
    | _ -> false

  (** Apply a value to another. *)
  let rec app u v =
    match u with
    | Abs (_, f) -> f v
    | Pi (_, t) -> t v
    | Neutral u -> Neutral (App (u, v))
    | _ -> assert false

  let cast u t =
    match u with
    | Neutral u -> Neutral (Cast (u, t))
    | u -> u
end

(** Evaluate an expression to a value. *)
let rec eval env e =
  match e.desc with
  | Let (p, v, e) ->
    let env = (p, eval env v)::env in
    eval env e
  | App (f, e) ->
    let f = eval env f in
    let e = eval env e in
    Value.app f e
  | Abs (p, t, e) ->
    let t = eval env t in
    Value.Abs (t, fun x -> eval ((p,x)::env) e)
  | Pi (p, t, e) ->
    let t = eval env t in
    Value.Pi (t, fun x -> eval ((p,x)::env) e)
  | Type -> Value.Type
  | Var x -> Value.Neutral (Value.Var x)
  | Cast (e, t) ->
    let e = eval env e in
    let t = eval env t in
    Value.cast e t
