%{
open Ast
%}

%token <int> INT_CONSTANT
%token <string> VARIABLE
%token PLUS
%token EOF

%left PLUS

%type <Ast.expr> expr
%start expr

%%

expr:
    | e = expr_inner EOF { e }
    ;

expr_inner:
    | INT_CONSTANT { Int $1 }
    | expr_inner PLUS expr_inner { Add ($1, $3) }
    | VARIABLE { Var $1 }
    ;
