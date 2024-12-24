%{
  open Ast
%}

%token AND OR NOT EQ
%token TRUE FALSE
%token <string> VAR
%token LPAREN RPAREN
%token LCURLY RCURLY COMMA
%token EOF

%start <Ast.assignment> cnf_with_assignments
%start <Ast.cnf> cnf_expr

%%

cnf_with_assignments:
  | LCURLY as_=assignments RCURLY f=cnf_expr EOF
    { { assignments = as_; formula = f } }
  ;

assignments:
  | { [] }
  | a=assignment COMMA as_=assignments { a :: as_ }
  | a=assignment { [a] }
  ;

assignment:
  | v=VAR EQ b=bool_const { (v, b) }
  ;

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

bool_const:
  | TRUE { true }
  | FALSE { false }
  ;
