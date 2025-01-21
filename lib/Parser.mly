%{
  open Ast
%}

%token AND OR NOT IMP EQ
%token TRUE FALSE
%token <string> VAR
%token LPAREN RPAREN
%token EOF

%start <Ast.boolean_expr> boolean_expr
%start <Ast.cnf> cnf_expr

%right IMP
%right EQ
%left OR
%left AND
%nonassoc NOT

%%

cnf_expr:
  | c=clause EOF { CNF [c] }
  | cs=cnf_clauses EOF { CNF cs }
  ;

cnf_clauses:
  | c=clause AND cs=cnf_clauses { c :: cs }
  | c=clause { [c] }
  ;

clause:
  | LPAREN ls=separated_nonempty_list(OR, literal) RPAREN { Clause ls }
  | l=literal { Clause [l] }
  ;

literal:
  | v=VAR { Pos v }
  | NOT v=VAR { Neg v }
  | b=bool_const { Bool b }
  ;

boolean_expr:
  | e=expr EOF { e }
  ;

expr:
  | e1=expr IMP e2=expr { Imp(e1, e2) }
  | e1=expr EQ e2=expr { Eq(e1, e2) }
  | e1=expr OR e2=expr { Or [e1; e2] }
  | e1=expr AND e2=expr { And [e1; e2] }
  | NOT e=expr { Not e }
  | LPAREN e=expr RPAREN { e }
  | v=VAR { Var v }
  | b=bool_const { Const b }
  ;

bool_const:
  | TRUE { true }
  | FALSE { false }
  ;
