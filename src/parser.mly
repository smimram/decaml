%{
open Preterm
open Module
%}

%token LET REC IN EQ COLON HOLE FUN TO INDUCTIVE BEGIN END
%token MATCH WITH BAR
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
  | INDUCTIVE name=IDENT ty=opttype EQ constructors=list(constructor) { Ind { name; parameters=[]; ty = Option.value ~default:(mk ~pos:$loc(ty) Type) ty; constructors } }

constructor:
  | BAR c=IDENT COLON ty=mexpr { (c,ty) }

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
  | LACC xx=list(IDENT) a=opttype RACC { List.map (fun x -> x, `Implicit, a) xx }
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
  | LACC xx=list(IDENT) a=opttype RACC { List.map (fun x -> x, `Implicit, a) xx }

expr:
  | MATCH t=expr WITH cases=list(case) { mk ~pos:$loc (Match (t, cases)) }
  | mexpr { $1 }

mexpr:
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
  | LPAR RPAR {mk ~pos:$loc (Var "uu") }
  | LPAR expr RPAR { $2 }
  | BEGIN expr END { $2 }

case:
  | BAR c=IDENT xx=list(IDENT) TO t=sexpr { (c, abss ~pos:$loc(t) (List.map (fun x -> x, `Explicit, None) xx) t) }
