type icit = [`Explicit | `Implicit]
[@@deriving show]

let icit_pa = function
  | `Explicit -> fun s -> "("^s^")"
  | `Implicit -> fun s -> "{"^s^"}"
