open Extlib

let error fmt = Printf.ksprintf (fun s -> print_endline s; exit 1) fmt

let () =
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  Sedlexing.set_filename lexbuf fname;
  let decls =
    try MenhirLib.Convert.Simplified.traditional2revised Parser.main @@ Sedlexing.with_tokenizer Lexer.token lexbuf
    with
    | Lexer.Error err ->
      let (sp, ep) = Sedlexing.lexing_positions lexbuf in
      error "Lexing error at %s: %s" (Pos.to_string (sp, ep)) err
    | Parser.Error -> error "Parse error at word \"%s\", %s." (Sedlexing.Utf8.lexeme lexbuf) (Pos.to_string @@ Sedlexing.lexing_positions lexbuf)
  in
  let decls = Module.prelude decls in
  close_in ic;
  try ignore @@ Module.eval Lang.Context.empty decls
  with
  | Lang.Type_error (pos, e) -> error "Type error at %s:\n%s\n%!" (Pos.to_string pos) e
