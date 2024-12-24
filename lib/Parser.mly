%{
open Ast
%}

%token <int> INT_CONSTANT
%token <string> VARIABLE
%token PLUS
%token MINUS
%token MUL
%token DIV
%token EOF

%left PLUS
%left MINUS
%left MUL
%left DIV

%type <Ast.expr> expr
%start expr

%%

expr:
  | e = expr_inner EOF { e }
  ;

expr_inner:
  | INT_CONSTANT { Int $1 }
  | expr_inner PLUS expr_inner { Add ($1, $3) }
  | expr_inner MINUS expr_inner { Sub ($1, $3) }
  | expr_inner MUL expr_inner { Mul ($1, $3) }
  | expr_inner DIV expr_inner { Div ($1, $3) }
  | VARIABLE { Var $1 }
  ;
