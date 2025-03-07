(** Terms are unevaluated expressions. Compared to pre-terms, variables are in de Bruijn indices. *)

open Common

(** An expression. *)
type t =
  | Abs of (string * icit) * t (** λ-abstraction *)
  | App of t * (icit * t)
  | Var of int
  | Pi of (string * icit * ty) * t
  | Meta of int
  | Type (** the type of types *)

  | Nat
  | Z | S | Ind_nat

and ty = t

let rec to_string vars = function
  | Abs ((x,i),t) ->
    let x = icit_pa i x in
    Printf.sprintf "fun %s -> %s" x (to_string (x::vars) t)
  | App (t,(i,u)) ->
    Printf.sprintf "%s %s" (to_string vars t) (icit_pa i (to_string vars u))
  | Var _ -> "x"
  | Pi ((x,i,a),_) ->
    let x = icit_pa i (x ^ " : " ^ to_string vars a) in
    Printf.sprintf "%s -> %s" x (to_string (x::vars) a)
  | Type -> "Type"
  | Meta m -> "?" ^ string_of_int m
  | Nat -> "Nat"
  | Z -> "Z"
  | S -> "S"
  | Ind_nat -> "Ind_nat"

let to_string = to_string []
