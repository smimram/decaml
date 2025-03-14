%{
open Preterm
open Module
%}

%token LET REC IN EQ COLON HOLE FUN TO
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
  | LET r=recursive f=IDENT args=args a=opttype EQ e=expr { (r, f, Option.map (pis ~pos:$loc args) a, abss ~pos:$loc args e) }

recursive:
  | REC { true }
  | { false }

args:
  | arg args { $1 @ $2 }
  | { [] }

arg:
  | LPAR x=IDENT a=opttype RPAR { [x, `Explicit, a] }
  | LACC xx=idents a=opttype RACC { List.map (fun x -> x, `Implicit, a) xx }
  | x=IDENT { [x, `Explicit, None] }
  | HOLE { ["_", `Explicit, None] }

opttype:
  | COLON expr { Some $2 }
  | { None }

piargs:
  | piarg piargs { $1 @ $2 }
  | piarg { $1 }

piarg:
  | LPAR x=IDENT COLON a=expr RPAR { [(x, `Explicit, Some a)] }
  | LACC xx=idents a=opttype RACC { List.map (fun x -> x, `Implicit, a) xx }

expr:
  | a=aexpr TO b=expr { arr ~pos:$loc a b }
  | a=piargs TO b=expr { pis ~pos:$loc a b }
  | FUN x=args TO t=expr { abss ~pos:$loc x t }
/* | LPAR IDENT COLON expr RPAR { mk ~pos:$loc (Cast (mk ~pos:$loc($2) (Var $2), $4)) } */
  | def IN u=expr { let (r, f, a, t) = $1 in assert (r = false); mk ~pos:$loc (Let (f, a, t, u)) }
  | aexpr { $1 }

// Application
aexpr:
  | aexpr sexpr { mk ~pos:$loc (App ($1, (`Explicit, $2))) }
  | aexpr LACC expr RACC { mk ~pos:$loc (App ($1, (`Implicit, $3))) }
  | sexpr { $1 }

// Simple expression
sexpr:
  | IDENT { mk ~pos:$loc (Var $1) }
  | TYPE { mk ~pos:$loc Type }
  | HOLE { mk ~pos:$loc Hole }
  | INT { nat ~pos:$loc $1 }
  | LPAR RPAR {mk ~pos:$loc U }
  | LPAR expr RPAR { $2 }

idents:
  | IDENT idents { $1::$2 }
  | IDENT { [$1] }
