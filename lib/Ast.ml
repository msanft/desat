type literal = Pos of string | Neg of string | Bool of bool
type clause = Clause of literal list
type cnf = CNF of clause list
type assignment = (string * bool) list (* simplified *)

let string_of_literal (lit : literal) : string =
  match lit with Pos v -> v | Neg v -> "!" ^ v | Bool b -> string_of_bool b

let string_of_clause (Clause lits : clause) : string =
  match lits with
  | [] -> "false"
  | [ lit ] -> string_of_literal lit
  | lits -> "(" ^ String.concat " || " (List.map string_of_literal lits) ^ ")"

let string_of_cnf (CNF clauses : cnf) : string =
  match clauses with
  | [] -> "true"
  | [ clause ] -> string_of_clause clause
  | clauses -> String.concat " && " (List.map string_of_clause clauses)

let string_of_assignment_pair (var, value) = var ^ " = " ^ string_of_bool value

let string_of_assignment (assgn : assignment) : string =
  String.concat ", " (List.map string_of_assignment_pair assgn)
