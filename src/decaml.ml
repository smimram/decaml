open Extlib

let () =
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let decls =
    try
      Parser.main Lexer.token lexbuf
    with
    | Failure err ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      failwith
        "Lexing error at line %d, character %d: %s"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        err
    | _ ->
      (* | Parsing.Parse_error -> *)
      let pos = (Lexing.lexeme_end_p lexbuf) in
      failwith
        "Parse error at word \"%s\", line %d, character %d."
        (Lexing.lexeme lexbuf)
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  in
  let decls = Preterm.prelude decls in
  close_in ic;
  try
    ignore @@
    List.fold_left
      (fun ctx decl ->
         match decl with
         | Preterm.Def (x,t) ->
           let _t, a = Lang.infer ctx t in
           Printf.printf "defined %s : %s\n%!" x (Value.to_string a);
           Lang.Context.bind ctx x a
      ) Lang.Context.empty decls
  with
  | Lang.Type_error (pos, e) ->
    failwith "Type error at %s: %s" (Pos.to_string pos) e
