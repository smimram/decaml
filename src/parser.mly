%{
open Preterm
open Module

let cast ~pos a t =
  match a with
  | Some a -> mk ~pos (Cast (t, a))
  | None -> t
%}

%token LET IN EQ COLON HOLE FUN TO
%token LPAR RPAR LACC RACC
%token TYPE
%token<string> IDENT
%token<int> INT
%token EOF

%start main
%type<Module.t> main
%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | { [] }

decl:
  | def { Def $1 }

def:
  | LET f=IDENT args=args a=opttype EQ e=expr { (f, abss ~pos:$loc args (cast ~pos:$loc(a) a e)) }

args:
  | arg args { $1 :: $2 }
  | { [] }

arg:
  | LPAR x=IDENT a=opttype RPAR { (x, `Explicit, a) }
  | LACC x=IDENT a=opttype RACC { (x, `Implicit, a) }
  | x=IDENT { (x, `Explicit, None) }

opttype:
  | COLON expr { Some $2 }
  | { None }

expr:
  | a=sexpr TO b=expr { arr ~pos:$loc a b }
  | FUN x=arg TO t=expr { mk ~pos:$loc (Abs (x, t)) }
  | aexpr { $1 }

// Application
aexpr:
  | aexpr sexpr { mk ~pos:$loc (App ($1, (`Explicit, $2))) }
  | sexpr { $1 }

// Simple expression
sexpr:
  | IDENT { mk ~pos:$loc (Var $1) }
  | TYPE { mk ~pos:$loc Type }
  | HOLE { mk ~pos:$loc Hole }
  | INT { nat ~pos:$loc $1 }
  | LPAR RPAR {mk ~pos:$loc U }
  | LPAR expr RPAR { $2 }
  /* | def IN expr { let (f, t) = $1 in mk ~pos:$loc (Let (f, None, t, $3)) } */
