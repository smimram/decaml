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
    | Failure err -> error "Lexing error at %s: %s" (Pos.to_string @@ Pos.lexeme lexbuf) err
    | Parser.Error -> error "Parse error at word \"%s\", %s." (Lexing.lexeme lexbuf) (Pos.to_string @@ Pos.lexeme lexbuf)
  in
  let decls = Module.prelude decls in
  close_in ic;
  try ignore @@ Module.eval Lang.Context.empty decls
  with
  | Lang.Type_error (pos, e) -> error "Type error at %s:\n%s\n%!" (Pos.to_string pos) e
