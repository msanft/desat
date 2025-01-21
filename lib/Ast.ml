type literal = Pos of string | Neg of string | Bool of bool
type clause = Clause of literal list
type cnf = CNF of clause list

type boolean_expr =
  | Var of string
  | Not of boolean_expr
  | And of boolean_expr list
  | Or of boolean_expr list
  | Eq of boolean_expr * boolean_expr
  | Imp of boolean_expr * boolean_expr
  | Const of bool

type assignment = (string * bool) list

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

let rec string_of_boolean_expr (f : boolean_expr) : string =
  match f with
  | Var v -> v
  | Not f -> (
      match f with
      | Var _ -> "!" ^ string_of_boolean_expr f
      | _ -> "!(" ^ string_of_boolean_expr f ^ ")")
  | And fs ->
      "(" ^ String.concat " && " (List.map string_of_boolean_expr fs) ^ ")"
  | Or fs ->
      "(" ^ String.concat " || " (List.map string_of_boolean_expr fs) ^ ")"
  | Eq (a, b) ->
      "(" ^ string_of_boolean_expr a ^ " <-> " ^ string_of_boolean_expr b ^ ")"
  | Imp (a, b) ->
      "(" ^ string_of_boolean_expr a ^ " -> " ^ string_of_boolean_expr b ^ ")"
  | Const b -> string_of_bool b
