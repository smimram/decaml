open Common

type level = int

type t =
  | Abs of (string * icit) * closure
  | Var of level * spine (* a variable applied to arguments *)
  | Pi of (string * icit * ty) * closure
  | Type

and ty = t

and environment = t list

(** A list of arguments. Important note this is reversed compared to the natural order: the first element of the list is the outermost argument! *)
and spine = (icit * t) list

and closure = environment * Term.t

(** Create a variable. *)
let var x = Var (x, [])

(** Generate a fresh variable name. *)
let fresh_var_name =
  let h = Hashtbl.create 100 in
  fun x ->
    let n = Option.value ~default:0 @@ Hashtbl.find_opt h x in
    Hashtbl.replace h x (n+1);
    x ^ "#" ^ string_of_int n
