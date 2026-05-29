open Extlib

type icit = [`Explicit | `Implicit]
[@@deriving show]

let icit_pa = function
  | `Explicit -> fun s -> "("^s^")"
  | `Implicit -> fun s -> "{"^s^"}"

let info kind fmt =
  Printf.ksprintf (fun s -> print_endline (kind ^ " " ^ s)) fmt

let debug kind fmt =
  Printf.ksprintf (fun s -> print_endline (Terminal.color `Yellow ^ kind ^ " " ^ s ^ Terminal.color `None)) fmt
