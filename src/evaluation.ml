module V = Value
module T = Term

type environment = V.environment
type tm = T.t
type value = V.t

(** Evaluate a term to a value. *)
let rec eval (env:environment) (t:tm) =
  match t with
  | Abs ((x,i),u) ->
    V.Abs ((x,i),(env,u))
  | App (t,(i,u)) ->
    let t = eval env t in
    let u = eval env u in
    app t (i,u)
  | Var x ->
    List.nth env x
  | Pi ((x,i,a),b) ->
    let a = eval env a in
    V.Pi ((x,i,a),(env,b))
  | Type ->
    V.Type

(** Apply a value to another *)
and app (t:value) u =
  match t with
  | Abs (_, _) -> _
  | Var (x, l) -> Var (x, u::l)
  | _ -> assert false
