(** Terms are unevaluated expressions. Compared to pre-terms, variables are in de Bruijn indices. *)

open Common

(** An expression. *)
type t =
  | Let of (string * ty * t * t)
  | Abs of (string * icit) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of int
  | Pi of (string * icit * ty) * t
  | Meta of meta
  | InsertedMeta of meta * [`Bound | `Defined] list
  | Fix of t
  | Type (** the type of types *)

  | Unit | U

  | Nat
  | Z | S | Ind_nat

and ty = t

and meta = int

let rec to_string ?(pa=false) vars t =
  let pa s = if pa then "("^s^")" else s in
  match t with
  | Let (x,a,t,u) ->
    pa @@ Printf.sprintf "let %s : %s = %s in\n%s" x (to_string vars a) (to_string vars t) (to_string vars u)
  | Abs ((x,i),t) ->
    let x = icit_pa i x in
    pa @@ Printf.sprintf "fun %s -> %s" x (to_string (x::vars) t)
  | App (t,(i,u)) ->
    let u =
      match i with
      | `Explicit -> to_string ~pa:true vars u
      | `Implicit -> icit_pa i @@ to_string vars u
    in
    pa @@ (to_string vars t ^ " " ^ u)
  | Var n -> if n < 0 then Printf.sprintf "x#%d" n else List.nth vars n
  | Pi ((x,i,a),b) ->
    let x = icit_pa i (x ^ " : " ^ to_string vars a) in
    pa @@ Printf.sprintf "%s -> %s" x (to_string (x::vars) b)
  | Fix t -> pa ("fix " ^ to_string vars t)
  | Type -> "type"
  | Meta m -> "?" ^ string_of_int m
  | InsertedMeta (m,_) -> "?" ^ string_of_int m
  | Unit -> "unit"
  | U -> "()"
  | Nat -> "nat"
  | Z -> "Z"
  | S -> "S"
  | Ind_nat -> "Ind_nat"

let abss xx t =
  let rec aux xx =
    match xx with
    | x::xx -> Abs (x, aux xx)
    | [] -> t
  in
  aux xx

let rec rev_apps_explicit t = function
  | u::uu -> App (rev_apps_explicit t uu, (`Explicit, u))
  | [] -> t
