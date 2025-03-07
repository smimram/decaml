%{
open Preterm
open Module
%}

%token LET IN EQ COLON HOLE
%token LPAR RPAR VBAR
%token TYPE
%token<string> IDENT
%token EOF

%start main
%type<Module.t list> main
%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | { [] }

decl:
  | def { Def $1 }

def:
  | LET f = IDENT a = args EQ e = expr { (f, abss ~pos:$loc a e) }

args:
  | arg args { $1 :: $2 }
  | { [] }

arg:
  | LPAR x = IDENT COLON a = expr RPAR { (x, `Explicit, a) }

expr:
  | expr sexpr { mk ~pos:$loc (App ($1, (`Explicit, $2))) }
  | sexpr { $1 }

sexpr:
  | IDENT { mk ~pos:$loc (Var $1) }
  | TYPE { mk ~pos:$loc Type }
  | HOLE { mk ~pos:$loc Hole }
  /* | def IN expr { let (f, t) = $1 in mk ~pos:$loc (App (mk ~pos:$loc($1) (Abs ((f, `Explicit, None), $3)), (`Explicit, t))) } */
