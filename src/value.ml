open Common

type level = int

type term = Term.t

type t =
  | Abs of (string * icit) * closure
  | Var of level * spine (* a variable applied to arguments *)
  | Meta of meta * spine
  | Pi of (string * icit * ty) * closure
  | Type

  | Unit | U
  | Nat | Z | S of t option | Ind_nat of t list

and ty = t

and environment = t list

(** A list of arguments. Important note this is reversed compared to the natural order: the first element of the list is the outermost argument! *)
and spine = (icit * t) list

and closure = environment * term

and meta =
  {
    id : int;
    mutable value : t option;
  }

(** Create a variable. *)
let var x = Var (x, [])

(** Generate a fresh variable name. *)
let fresh_var_name =
  let h = Hashtbl.create 100 in
  fun x ->
    let n = Option.value ~default:0 @@ Hashtbl.find_opt h x in
    Hashtbl.replace h x (n+1);
    x ^ "#" ^ string_of_int n

(** Replace metavariables by their value. *)
let force (t:t) = t (* TODO *)

(** Evaluate a term to a value. *)
let rec eval (env:environment) (t:term) =
  match t with
  | Let (_,_,t,u) ->
    let t = eval env t in
    eval (t::env) u
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
  | Meta m -> Meta ({ id = m; value = None }, [])
  | Type -> Type
  | Unit -> Unit
  | U -> U
  | Nat -> Nat
  | Z -> Z
  | S -> S None
  | Ind_nat -> Ind_nat []

(** Apply a value to another *)
and app (t:t) u =
  match t with
  (* | Abs (_, _) -> _ *)
  | Var (x,s) -> Var (x, u::s)
  | _ -> assert false

(** Reify a value as a term. *)
let rec quote l (t:t) : term =
  let rec app_spine t : spine -> term = function
    | (i,u)::s -> App (app_spine t s, (i, quote l u))
    | [] -> t
  in
  let rec app_explicit_spine t : t list -> term = function
    | u::s -> App (app_explicit_spine t s, (`Explicit, quote l u))
    | [] -> t
  in
  match t with
  | Abs ((x,i),(env,t)) ->
    let t = quote (l+1) @@ eval ((var l)::env) t in
    Abs ((x,i),t)
  | Var (x,s) ->
    app_spine (Var (l-x-1)) s
  | Pi ((x,i,a),(env,b)) ->
    let a = quote l a in
    let b = quote (l+1) @@ eval ((var l)::env) b in
    Pi ((x,i,a),b)
  | Meta (m, s) ->
    app_spine (Meta m.id) s
  | Type -> Type
  | Unit -> Unit
  | U -> U
  | Nat -> Nat
  | Z -> Z
  | S None -> S
  | S (Some t) -> App (S, (`Explicit, quote l t))
  | Ind_nat s -> app_explicit_spine Ind_nat s

let to_string ?(vars=[]) t = Term.to_string ~vars @@ quote 0 t

exception Unification

(** Unify two values. *)
let rec unify l (t:t) (u:t) =
  match t, u with
  | Abs ((_,i),(env,b)), Abs ((_,i'),(env',b')) ->
    if i <> i' then raise Unification;
    let b = eval ((var l)::env) b in
    let b' = eval ((var l)::env') b' in
    unify (l+1) b b'
  | Type, Type -> ()
  | _ -> raise Unification

let unify l t u =
  try unify l t u; true
  with Unification -> false
