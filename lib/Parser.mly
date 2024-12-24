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

%type <Ast.cnf_expr> cnf_expr
%type <Ast.cnf_expr list> cnf_expr_list
%start cnf_expr
%start cnf_expr_list

%%

cnf_expr:
  | e = cnf_expr_inner EOF { e }
  ;

cnf_expr_list:
  | es = cnf_expr_sequence EOF { es }
  ;

cnf_expr_sequence:
  | e = cnf_expr_inner { [e] }                               (* Single cnf_expression *)
  | e = cnf_expr_inner COMMA es = cnf_expr_sequence { e :: es }  (* Multiple cnf_expressions *)
  ;

cnf_expr_inner:
  | BOOL { Bool $1 }
  | LPAREN e = cnf_expr_inner RPAREN { e }
  | cnf_expr_inner AND cnf_expr_inner { And ($1, $3) }
  | cnf_expr_inner OR cnf_expr_inner { Or ($1, $3) }
  | NOT cnf_expr_inner { Not $2 }
  | VARIABLE { Var $1 }
  ;
