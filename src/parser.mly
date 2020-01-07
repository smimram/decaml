%{
    open Lang
%}

%token LET IN EQ COLON IND
%token LPAR RPAR VBAR
%token TYPE
%token<string> IDENT
%token EOF

%start main
%type<Lang.decl list> main
%%

main:
  | decls EOF { $1 }

decls:
  | decl decls { $1 :: $2 }
  | { [] }

decl:
  | def { Def $1 }
  | ind { Ind $1 }

def:
  | LET f = IDENT a = args COLON t = expr EQ e = expr { (PVar f, abss ~pos:$loc a (cast ~pos:e.pos e t)) }
  | LET f = IDENT a = args EQ e = expr { (PVar f, abss ~pos:$loc a e) }

args:
  | arg args { $1 :: $2 }
  | { [] }

arg:
  | LPAR p = pattern COLON e = expr RPAR { (p, e) }

pattern:
  | IDENT { PVar $1 }

expr:
  | def IN expr { letin ~pos:$loc $1 $3 }
  | IDENT { var ~pos:$loc $1 }
  | TYPE { typ ~pos:$loc () }

ind:
  | IND name = IDENT EQ cons = constructors { { ind_name = name; ind_param = []; ind_type = typ (); ind_cons = cons } }

constructors:
  | { [] }
  | constructor constructors { $1::$2 }

constructor:
  | VBAR cons = IDENT COLON a = expr { (cons, a) }
