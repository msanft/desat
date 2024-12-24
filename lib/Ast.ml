type cnf_expr =
  | Bool of bool
  | And of cnf_expr * cnf_expr
  | Or of cnf_expr * cnf_expr
  | Not of cnf_expr
  | Var of string

let rec dump_cnf_expr (e : cnf_expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | And (e1, e2) -> "(" ^ dump_cnf_expr e1 ^ " && " ^ dump_cnf_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ dump_cnf_expr e1 ^ " || " ^ dump_cnf_expr e2 ^ ")"
  | Not e1 -> "!" ^ dump_cnf_expr e1
  | Var s -> s

let dump_cnf_expr_list (cnf_exprs : cnf_expr list) : string =
  String.concat ", " (List.map dump_cnf_expr cnf_exprs)
