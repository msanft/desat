%{
open Ast
%}

%token <string> VARIABLE
%token <bool> BOOL
%token AND
%token OR
%token NOT
%token LPAREN
%token RPAREN
%token COMMA
%token EOF

%left OR
%left AND
%right NOT

%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%start expr
%start expr_list

%%

expr:
  | e = expr_inner EOF { e }
  ;

expr_list:
  | es = expr_sequence EOF { es }
  ;

expr_sequence:
  | e = expr_inner { [e] }                               (* Single expression *)
  | e = expr_inner COMMA es = expr_sequence { e :: es }  (* Multiple expressions *)
  ;

expr_inner:
  | BOOL { Bool $1 }
  | LPAREN e = expr_inner RPAREN { e }
  | expr_inner AND expr_inner { And ($1, $3) }
  | expr_inner OR expr_inner { Or ($1, $3) }
  | NOT expr_inner { Not $2 }
  | VARIABLE { Var $1 }
  ;
