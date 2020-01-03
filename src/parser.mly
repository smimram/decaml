%{
    open Lang
%}

%token LET IN EQ COLON
%token LPAR RPAR
%token TYPE
%token<string> IDENT
%token EOF

%start main
%type<(string * Lang.t) list> main
%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | { [] }

decl:
  | LET f = IDENT a = args COLON t = expr EQ e = expr { (f, abss ~pos:$loc a (cast ~pos:e.pos e t)) }

args:
  | arg args { $1 :: $2 }
  | { [] }

arg:
  | LPAR p = pattern COLON e = expr RPAR { (p, e) }

pattern:
  | IDENT { PVar $1 }

expr:
  | IDENT { var ~pos:$loc $1 }
  | TYPE { typ ~pos:$loc () }
