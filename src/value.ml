open Common

type level = int

type term = Term.t

type t =
  | Abs of (string * icit) * closure
  | Var of level * spine (* a variable applied to arguments *)
  | Pi of (string * icit * ty) * closure
  | Type

and ty = t

and environment = t list

(** A list of arguments. Important note this is reversed compared to the natural order: the first element of the list is the outermost argument! *)
and spine = (icit * t) list

and closure = environment * term

(** Create a variable. *)
let var x = Var (x, [])

(** Generate a fresh variable name. *)
let fresh_var_name =
  let h = Hashtbl.create 100 in
  fun x ->
    let n = Option.value ~default:0 @@ Hashtbl.find_opt h x in
    Hashtbl.replace h x (n+1);
    x ^ "#" ^ string_of_int n

(** Evaluate a term to a value. *)
let rec eval (env:environment) (t:term) =
  match t with
  | Abs ((x,i),u) ->
    Abs ((x,i),(env,u))
  | App (t,(i,u)) ->
    let t = eval env t in
    let u = eval env u in
    app t (i,u)
  | Var x ->
    List.nth env x
  | Pi ((x,i,a),b) ->
    let a = eval env a in
    Pi ((x,i,a),(env,b))
  | Type ->
    Type

(** Apply a value to another *)
and app (t:t) u =
  match t with
  (* | Abs (_, _) -> _ *)
  | Var (x,s) -> Var (x, u::s)
  | _ -> assert false

(** Reify a value as a term. *)
let rec quote l (t:t) : term =
  match t with
  | Abs ((x,i),(env,t)) ->
    let t = quote (l+1) (eval ((var l)::env) t) in
    Abs ((x,i),t)
  | Var (x,s) ->
    let rec aux : spine -> term = function
      | (i,t)::s -> App (aux s, (i, quote l t))
      | [] -> Var x
    in
    aux s
  | Pi ((x,i,a),(env,b)) ->
    let a = quote l a in
    let b = quote (l+1) (eval ((var l)::env) b) in
    Pi ((x,i,a),b)
  | Type -> Type
