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
  let decls = Module.prelude decls in
  close_in ic;
  try
    ignore @@
    List.fold_left
      (fun ctx decl ->
         match decl with
         | Module.Def (x,t) ->
           (* Printf.printf "%s = %s\n%!" x (Preterm.to_string t); *)
           let t, a = Lang.infer ctx t in
           Printf.printf "%s : %s\n%!" x (Value.to_string ~vars:(List.map fst ctx.types) a);
           Printf.printf "%s = %s\n%!" x (Term.to_string ~vars:(List.map fst ctx.types) t);
           print_newline ();
           (* Lang.Context.bind ctx x a *)
           Lang.Context.define ctx x (Value.eval ctx.environment t) a
         | Module.Ind ind ->
           Printf.printf "inductive %s" ind.Preterm.name;
           let ind : Value.inductive =
             { Value.
               name = ind.name;
               ty = Value.eval ctx.environment ty;
               constructors = List.map (fun (c,a) -> c, Value.eval ctx.environment a) ind.constructors;
             }
           in
           Lang.Context.inductive ctx ind
      ) Lang.Context.empty decls
  with
  | Lang.Type_error (pos, e) ->
    Printf.printf "Type error at %s:\n%s\n%!" (Pos.to_string pos) e;
    exit 1
