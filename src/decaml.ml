open Extlib

let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt

let () =
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let decls =
    try Parser.main Lexer.token lexbuf
    with
    | Failure err ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      error
        "Lexing error at line %d, character %d: %s"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        err
    | _ ->
      (* | Parsing.Parse_error -> *)
      let pos = (Lexing.lexeme_end_p lexbuf) in
      error
        "Parse error at word \"%s\", line %d, character %d."
        (Lexing.lexeme lexbuf)
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  in
  let decls = Module.prelude decls in
  close_in ic;
  try ignore @@ Module.eval Lang.Context.empty decls
  with
  | Lang.Type_error (pos, e) -> error "Type error at %s:\n%s\n%!" (Pos.to_string pos) e
