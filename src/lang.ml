module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy : t = Lexing.dummy_pos, Lexing.dummy_pos
end

(** A variable. *)
type var = string

(** A constructor. *)
type cons = string

module Expr = struct
  type t =
    {
      pos : Pos.t;
      desc : desc;
    }

  and desc =
    | Let of decl * t
    | Abs of pattern * t * t
    | App of t * t
    | Pi of pattern * t * t
    | Var of var
    | Cast of t * t (** cast an expression to a given type *)
    (* | Ind of (cons * t list) list (\** an inductive type *\) *)
    | Type

  and pattern =
    | PVar of var

  and decl = pattern * t

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

  (** Multiple typed abstractions. *)
  let abss ?pos a e =
    let pos = Option.value ~default:e.pos pos in
    let rec aux = function
      | [] -> e
      | (x,t)::l -> abs ~pos x t (aux l)
    in
    aux a

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

  let var x = Neutral (Var x)

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

  let fresh i = "x#" ^ string_of_int i

  let rec readback i v =
    let rec neutral i = function
      | App (u, v) ->
        let u = neutral i u in
        let v = readback i v in
        Expr.app u v
      | Var x -> Expr.var x
      | Cast (u, t) ->
        let u = neutral i u in
        let t = readback i t in
        Expr.cast u t
    in
    match v with
    | Abs (t, f) ->
      let t = readback i t in
      let x = fresh i in
      let e = readback (i+1) (f (var x)) in
      Expr.abs (PVar x) t e
    | Pi (t, f) ->
      let t = readback i t in
      let x = fresh i in
      let e = readback (i+1) (f (var x)) in
      Expr.pi (PVar x) t e
    | Type -> Expr.typ ()
    | Neutral u -> neutral i u

  (** Convertibility of values. *)
  (* TODO: could be improved by being coded directly. *)
  let eq i t u = readback i t = readback i u

  let to_string v =
    to_string (readback 0 v)

  (** Evaluation environment. *)
  module Env = struct
    type nonrec t = (var * t) list

    let empty : t = []

    let add env p t =
      match p with
      | PVar x -> (x,t)::env

    let get env x = List.assoc x env
  end
end

module V = Value

(** Evaluate an expression to a value. *)
let rec eval env e =
  match e.desc with
  | Let ((p, v), e) ->
    let env = V.Env.add env p (eval env v) in
    eval env e
  | App (f, e) ->
    let f = eval env f in
    let e = eval env e in
    V.app f e
  | Abs (p, t, e) ->
    let t = eval env t in
    V.Abs (t, fun x -> eval (V.Env.add env p x) e)
  | Pi (p, t, e) ->
    let t = eval env t in
    V.Pi (t, fun x -> eval (V.Env.add env p x) e)
  | Type -> V.Type
  | Var x ->
    (
      try V.Env.get env x
      with Not_found -> V.Neutral (V.Var x)
    )
  | Cast (e, t) ->
    let e = eval env e in
    let t = eval env t in
    V.cast e t

(** Typing environment. *)
module Env = struct
  type nonrec t = (var * V.t) list * V.Env.t (** typing and evaluation environment *)

  let empty : t = [], V.Env.empty

  let add ((tenv, env):t) p a t =
    let tenv =
      match p, a with
      | PVar x, a -> (x,a)::tenv
    in
    tenv, V.Env.add env p t

  let get_type ((tenv, env):t) x = List.assoc x tenv

  let get_value ((tenv, env):t) x = V.Env.get x env

  let eval ((tenv, env):t) = env
end

let eval env t = eval (Env.eval env) t

exception Type_error of Pos.t * string

let type_error pos =
  Printf.kprintf (fun s -> raise (Type_error (pos, s)))

(** Infer the type of an expression. *)
let rec infer i (env : Env.t) t =
  (* Printf.printf "INFER %s\n%!" (to_string t); *)
  let pos = t.pos in
  match t.desc with
  | Let ((p, t), u) ->
    let a = infer i env t in
    let t = eval env t in
    let env = Env.add env p a t in
    infer i env u
  | Var x ->
    (
      try Env.get_type env x
      with Not_found -> type_error pos "unbound variable %s" x
    )
  | Abs (p, a, t) ->
    let i = i+1 in
    let a = eval env a in
    (
      let x = V.var (V.fresh i) in
      let env = Env.add env p a x in
      let b = infer i env t in
      ignore b
    );
    let b u =
      let env = Env.add env p a u in
      infer i env t
    in
    V.Pi (a, b)
  | App (t, u) ->
    (
      match infer i env t with
      | V.Pi (a, b) ->
        check i env u a;
        b (eval env u)
      | _ -> type_error t.pos "function expected"
    )
  | Pi (p, a, b) ->
    check i env a V.Type;
    let a = eval env a in
    let x = V.var (V.fresh i) in
    let i = i+1 in
    let env = Env.add env p a x in
    check i env b V.Type;
    V.Type
  | Cast (t, a) ->
    check i env a V.Type;
    let a = eval env a in
    check i env t a;
    a
  | Type -> V.Type

(** Check that an expression has given type. *)
and check i env t a =
  Printf.printf "CHECK %s IS %s\n%!" (to_string t) (V.to_string a);
  let pos = t.pos in
  match t.desc, a with
  | Abs (p, a', t), V.Pi (a, b) ->
    let a' = eval env a' in
    if not (V.eq i a' a) then type_error pos "unexpected abstraction type";
    let x = V.var (V.fresh i) in
    let i = i+1 in
    let env = Env.add env p a x in
    check i env t (b x)
  | _, _ ->
    let a' = infer i env t in
    if not (V.eq i a' a) then type_error pos "wrong type"
